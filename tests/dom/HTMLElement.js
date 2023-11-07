// @flow

let tests = [
  // hasAttributes
  function (element: HTMLElement) {
    element.hasAttributes();

    // fails
    element.hasAttributes('foo', 'bar');
  },

  // scrollIntoView
  function (element: HTMLElement) {
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
  function (element: HTMLElement) {
    const str: string = 'a';
    // broad
    element.getElementsByTagName(str) as HTMLCollection<HTMLElement>;
    element.getElementsByTagNameNS(null, str) as HTMLCollection<HTMLElement>;
    element.querySelector(str) as HTMLElement | null;
    element.querySelectorAll(str) as NodeList<HTMLElement>;

    // specific
    element.getElementsByTagName('a') as HTMLCollection<HTMLAnchorElement>;
    element.getElementsByTagNameNS(
      null,
      'a',
    ) as HTMLCollection<HTMLAnchorElement>;
    element.querySelector('a') as HTMLAnchorElement | null;
    element.querySelectorAll('a') as NodeList<HTMLAnchorElement>;

    // overly broad input (fails)
    element.getElementsByTagName(str) as HTMLCollection<HTMLAnchorElement>;
    element.getElementsByTagNameNS(
      null,
      str,
    ) as HTMLCollection<HTMLAnchorElement>;
    element.querySelector(str) as HTMLAnchorElement | null;
    element.querySelectorAll(str) as NodeList<HTMLAnchorElement>;

    // wrong specific input (fails)
    element.getElementsByTagName('div') as HTMLCollection<HTMLAnchorElement>;
    element.getElementsByTagNameNS(
      null,
      'div',
    ) as HTMLCollection<HTMLAnchorElement>;
    element.querySelector('div') as HTMLAnchorElement | null;
    element.querySelectorAll('div') as NodeList<HTMLAnchorElement>;
  },

  // focus
  function (element: HTMLElement) {
    element.focus();
    element.focus({});
    element.focus({preventScroll: true});
    element.focus({preventScroll: false});

    // fails
    element.focus({preventScroll: 'invalid'});
    element.focus(1);
  },
];
