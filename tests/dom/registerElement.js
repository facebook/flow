// @flow

let tests = [
  // should work with Object.create()
  function() {
    document.registerElement('custom-element', {
      prototype: Object.create(HTMLElement.prototype, {
        createdCallback: { value: function createdCallback () {
        }},
        attachedCallback: { value: function attachedCallback () {
        }},
        detachedCallback: { value: function detachedCallback () {
        }},
        attributeChangedCallback: {
          value: function attributeChangedCallback (
            attributeLocalName: string,
            oldAttributeValue: string | null,
            newAttributeValue: string | null,
            attributeNamespace: string,
          ) {
          }
        }
      })
    })
  },
  // should complain about invalid callback parameters
  function() {
    document.registerElement('custom-element', {
      prototype: {
        attributeChangedCallback(
          localName: string,
          oldVal: string, // Error: This might be null
          newVal: string, // Error: This might be null
          namespace: string) {}
      },
    });
  },
];
