// @flow

let tests = [
  // hasAttributes
  function(element: HTMLElement) {
    element.hasAttributes();

    // fails
    element.hasAttributes('foo', 'bar');
  },

  // scrollIntoView
  function(element: HTMLElement) {
    element.scrollIntoView();
    element.scrollIntoView(false);
    element.scrollIntoView({});
    element.scrollIntoView({behavior: 'smooth', block: 'end'});
    element.scrollIntoView({block: 'end'});
    element.scrollIntoView({behavior: 'smooth'});

    // fails
    element.scrollIntoView({behavior: 'invalid'});
    element.scrollIntoView({block: 'invalid'});
    element.scrollIntoView(1);
  },

  // functions with specific overloads
  function(element: HTMLElement) {
    const str: string = 'a';
    // broad
    (element.getElementsByTagName(str): HTMLCollection<HTMLElement>);
    (element.getElementsByTagNameNS(null, str): HTMLCollection<HTMLElement>);
    (element.querySelector(str): HTMLElement | null);
    (element.querySelectorAll(str): NodeList<HTMLElement>);

    // specific
    (element.getElementsByTagName('a'): HTMLCollection<HTMLAnchorElement>);
    (element.getElementsByTagNameNS(
      null,
      'a',
    ): HTMLCollection<HTMLAnchorElement>);
    (element.querySelector('a'): HTMLAnchorElement | null);
    (element.querySelectorAll('a'): NodeList<HTMLAnchorElement>);

    // overly broad input (fails)
    (element.getElementsByTagName(str): HTMLCollection<HTMLAnchorElement>);
    (element.getElementsByTagNameNS(
      null,
      str,
    ): HTMLCollection<HTMLAnchorElement>);
    (element.querySelector(str): HTMLAnchorElement | null);
    (element.querySelectorAll(str): NodeList<HTMLAnchorElement>);

    // wrong specific input (fails)
    (element.getElementsByTagName('div'): HTMLCollection<HTMLAnchorElement>);
    (element.getElementsByTagNameNS(
      null,
      'div',
    ): HTMLCollection<HTMLAnchorElement>);
    (element.querySelector('div'): HTMLAnchorElement | null);
    (element.querySelectorAll('div'): NodeList<HTMLAnchorElement>);
  },

  // focus
  function(element: HTMLElement) {
    element.focus();
    element.focus({});
    element.focus({preventScroll: true});
    element.focus({preventScroll: false});

    // fails
    element.focus({preventScroll: 'invalid'});
    element.focus(1);
  },
];
