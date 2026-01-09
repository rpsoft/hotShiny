/**
 * DOM Diffing Engine
 * Efficiently updates DOM by diffing virtual DOM representations
 */

class VirtualDOM {
  constructor(tag, props = {}, children = []) {
    this.tag = tag;
    this.props = props;
    this.children = children;
    this.key = props.key;
  }
  
  static fromElement(element) {
    if (element.nodeType === Node.TEXT_NODE) {
      return element.textContent;
    }
    
    const tag = element.tagName.toLowerCase();
    const props = {};
    const children = [];
    
    // Copy attributes
    for (let attr of element.attributes) {
      props[attr.name] = attr.value;
    }
    
    // Process children
    for (let child of element.childNodes) {
      if (child.nodeType === Node.TEXT_NODE) {
        children.push(child.textContent);
      } else {
        children.push(VirtualDOM.fromElement(child));
      }
    }
    
    return new VirtualDOM(tag, props, children);
  }
  
  toElement() {
    if (typeof this === 'string') {
      return document.createTextNode(this);
    }
    
    const element = document.createElement(this.tag);
    
    // Set attributes
    for (let key in this.props) {
      if (key === 'key') continue;
      element.setAttribute(key, this.props[key]);
    }
    
    // Add children
    for (let child of this.children) {
      if (typeof child === 'string') {
        element.appendChild(document.createTextNode(child));
      } else {
        element.appendChild(child.toElement());
      }
    }
    
    return element;
  }
}

class DOMDiff {
  static diff(oldVNode, newVNode) {
    const patches = [];
    
    if (!oldVNode && newVNode) {
      // New node
      patches.push({ type: 'CREATE', node: newVNode });
    } else if (oldVNode && !newVNode) {
      // Removed node
      patches.push({ type: 'REMOVE', node: oldVNode });
    } else if (this.isTextNode(oldVNode) || this.isTextNode(newVNode)) {
      // Text node
      if (oldVNode !== newVNode) {
        patches.push({ type: 'TEXT', node: newVNode });
      }
    } else if (oldVNode.tag !== newVNode.tag) {
      // Different tags
      patches.push({ type: 'REPLACE', oldNode: oldVNode, newNode: newVNode });
    } else {
      // Same tag, diff props and children
      const propsPatches = this.diffProps(oldVNode.props, newVNode.props);
      if (propsPatches.length > 0) {
        patches.push({ type: 'PROPS', patches: propsPatches });
      }
      
      const childrenPatches = this.diffChildren(oldVNode.children, newVNode.children);
      if (childrenPatches.length > 0) {
        patches.push({ type: 'CHILDREN', patches: childrenPatches });
      }
    }
    
    return patches;
  }
  
  static diffProps(oldProps, newProps) {
    const patches = [];
    
    // Find changed and removed props
    for (let key in oldProps) {
      if (!(key in newProps)) {
        patches.push({ type: 'REMOVE', key: key });
      } else if (oldProps[key] !== newProps[key]) {
        patches.push({ type: 'SET', key: key, value: newProps[key] });
      }
    }
    
    // Find new props
    for (let key in newProps) {
      if (!(key in oldProps)) {
        patches.push({ type: 'SET', key: key, value: newProps[key] });
      }
    }
    
    return patches;
  }
  
  static diffChildren(oldChildren, newChildren) {
    const patches = [];
    const maxLength = Math.max(oldChildren.length, newChildren.length);
    
    for (let i = 0; i < maxLength; i++) {
      const oldChild = oldChildren[i];
      const newChild = newChildren[i];
      
      if (!oldChild && newChild) {
        patches.push({ type: 'CREATE', index: i, node: newChild });
      } else if (oldChild && !newChild) {
        patches.push({ type: 'REMOVE', index: i });
      } else {
        const childPatches = this.diff(oldChild, newChild);
        if (childPatches.length > 0) {
          patches.push({ type: 'UPDATE', index: i, patches: childPatches });
        }
      }
    }
    
    return patches;
  }
  
  static isTextNode(node) {
    return typeof node === 'string';
  }
}

class DOMPatcher {
  constructor(rootElement) {
    this.root = rootElement;
    this.currentVNode = null;
  }
  
  patch(patches) {
    this.applyPatches(this.root, patches);
  }
  
  applyPatches(node, patches) {
    for (let patch of patches) {
      switch (patch.type) {
        case 'CREATE':
          if (typeof patch.node === 'string') {
            node.appendChild(document.createTextNode(patch.node));
          } else {
            node.appendChild(patch.node.toElement());
          }
          break;
          
        case 'REMOVE':
          if (patch.index !== undefined) {
            const child = node.childNodes[patch.index];
            if (child) {
              node.removeChild(child);
            }
          } else {
            node.parentNode.removeChild(node);
          }
          break;
          
        case 'TEXT':
          node.textContent = patch.node;
          break;
          
        case 'REPLACE':
          const newElement = patch.newNode.toElement();
          node.parentNode.replaceChild(newElement, node);
          break;
          
        case 'PROPS':
          this.applyPropsPatches(node, patch.patches);
          break;
          
        case 'CHILDREN':
          this.applyChildrenPatches(node, patch.patches);
          break;
      }
    }
  }
  
  applyPropsPatches(element, patches) {
    for (let patch of patches) {
      switch (patch.type) {
        case 'SET':
          if (patch.key === 'key') continue;
          element.setAttribute(patch.key, patch.value);
          break;
        case 'REMOVE':
          element.removeAttribute(patch.key);
          break;
      }
    }
  }
  
  applyChildrenPatches(element, patches) {
    for (let patch of patches) {
      switch (patch.type) {
        case 'CREATE':
          const newChild = typeof patch.node === 'string' 
            ? document.createTextNode(patch.node)
            : patch.node.toElement();
          if (patch.index >= element.childNodes.length) {
            element.appendChild(newChild);
          } else {
            element.insertBefore(newChild, element.childNodes[patch.index]);
          }
          break;
          
        case 'REMOVE':
          const child = element.childNodes[patch.index];
          if (child) {
            element.removeChild(child);
          }
          break;
          
        case 'UPDATE':
          const childNode = element.childNodes[patch.index];
          if (childNode) {
            this.applyPatches(childNode, patch.patches);
          }
          break;
      }
    }
  }
  
  update(newVNode) {
    if (!this.currentVNode) {
      // First render
      this.currentVNode = newVNode;
      const element = newVNode.toElement();
      this.root.appendChild(element);
    } else {
      // Diff and patch
      const patches = DOMDiff.diff(this.currentVNode, newVNode);
      this.patch(patches);
      this.currentVNode = newVNode;
    }
  }
}

// Export for use in other modules
if (typeof module !== 'undefined' && module.exports) {
  module.exports = { VirtualDOM, DOMDiff, DOMPatcher };
}
