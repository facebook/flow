/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import React, {useEffect, useState, useRef} from 'react';
import clsx from 'clsx';
import Highlight, {defaultProps} from 'prism-react-renderer';
import copy from 'copy-text-to-clipboard';
import usePrismTheme from '@theme/hooks/usePrismTheme';
import Translate, {translate} from '@docusaurus/Translate';
import styles from './FlowCheckCodeBlock.module.css';
import {useThemeConfig} from '@docusaurus/theme-common';

export default function FlowCheckCodeBlock({children, flowErrors}) {
  const {prism} = useThemeConfig();
  const [showCopied, setShowCopied] = useState(false);
  const [mounted, setMounted] = useState(false);
  // The Prism theme on SSR is always the default theme but the site theme
  // can be in a different mode. React hydration doesn't update DOM styles
  // that come from SSR. Hence force a re-render after mounting to apply the
  // current relevant styles. There will be a flash seen of the original
  // styles seen using this current approach but that's probably ok. Fixing
  // the flash will require changing the theming approach and is not worth it
  // at this point.

  useEffect(() => {
    setMounted(true);
  }, []);

  const button = useRef(null);
  const prismTheme = usePrismTheme();
  // In case interleaved Markdown (e.g. when using CodeBlock as standalone component).

  const content = Array.isArray(children) ? children.join('') : children;

  const language = 'jsx';
  const code = content.replace(/\n$/, '');

  const handleCopyCode = () => {
    copy(code);
    setShowCopied(true);
    setTimeout(() => setShowCopied(false), 2000);
  };

  return (
    <Highlight
      {...defaultProps}
      key={String(mounted)}
      theme={prismTheme}
      code={code}
      language={language}>
      {({className, style, tokens, getLineProps, getTokenProps}) => (
        <div className={styles.codeBlockContainer}>
          <div className={clsx(styles.codeBlockContent, language)}>
            <pre
              /* eslint-disable-next-line jsx-a11y/no-noninteractive-tabindex */
              tabIndex={0}
              className={clsx(className, styles.codeBlock, 'thin-scrollbar')}
              style={style}>
              <code className={styles.codeBlockLines}>
                {tokens.map((line, i) => {
                  if (line.length === 1 && line[0].content === '') {
                    line[0].content = '\n'; // eslint-disable-line no-param-reassign
                  }

                  const lineProps = getLineProps({
                    line,
                    key: i,
                  });

                  lineProps.className += styles.codeBlockLine;

                  return (
                    <span key={i} {...lineProps}>
                      <span className={styles.codeBlockLineNumber}>
                        {i + 1}
                      </span>
                      <span className={styles.codeBlockLineContent}>
                        {line.map((token, key) => (
                          <span key={key} {...getTokenProps({token, key})} />
                        ))}
                      </span>
                    </span>
                  );
                })}
              </code>
            </pre>

            <button
              ref={button}
              type="button"
              aria-label={translate({
                id: 'theme.CodeBlock.copyButtonAriaLabel',
                message: '',
                description: 'The ARIA label for copy code blocks button',
              })}
              className={clsx(styles.copyButton, 'clean-btn')}
              onClick={handleCopyCode}>
              {showCopied ? (
                <Translate
                  id="theme.CodeBlock.copied"
                  description="The copied button label on code blocks">
                  Copied
                </Translate>
              ) : (
                <Translate
                  id="theme.CodeBlock.copy"
                  description="The copy button label on code blocks">
                  Copy
                </Translate>
              )}
            </button>

            {flowErrors.length > 0 && (
              <pre className={styles.flowErrors}>
                {flowErrors.map((error, index) => (
                  <div key={index}>{error}</div>
                ))}
              </pre>
            )}
          </div>
        </div>
      )}
    </Highlight>
  );
}
