/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import React from 'react';
import Link from '@docusaurus/Link';
import styles from './DocsLandingPage.module.css';

/* Inline SVG icons — lightweight, crisp at any size, match section colors */
const icons = {
  rocket: (
    <svg
      viewBox="0 0 24 24"
      fill="none"
      stroke="currentColor"
      strokeWidth="1.5"
      strokeLinecap="round"
      strokeLinejoin="round">
      <path d="M4.5 16.5c-1.5 1.26-2 5-2 5s3.74-.5 5-2c.71-.84.7-2.13-.09-2.91a2.18 2.18 0 0 0-2.91-.09z" />
      <path d="M12 15l-3-3a22 22 0 0 1 2-3.95A12.88 12.88 0 0 1 22 2c0 2.72-.78 7.5-6 11a22.35 22.35 0 0 1-4 2z" />
      <path d="M9 12H4s.55-3.03 2-4c1.62-1.08 5 0 5 0" />
      <path d="M12 15v5s3.03-.55 4-2c1.08-1.62 0-5 0-5" />
    </svg>
  ),
  help: (
    <svg
      viewBox="0 0 24 24"
      fill="none"
      stroke="currentColor"
      strokeWidth="1.5"
      strokeLinecap="round"
      strokeLinejoin="round">
      <circle cx="12" cy="12" r="10" />
      <path d="M9.09 9a3 3 0 0 1 5.83 1c0 2-3 3-3 3" />
      <line x1="12" y1="17" x2="12.01" y2="17" />
    </svg>
  ),
  code: (
    <svg
      viewBox="0 0 24 24"
      fill="none"
      stroke="currentColor"
      strokeWidth="1.5"
      strokeLinecap="round"
      strokeLinejoin="round">
      <polyline points="16 18 22 12 16 6" />
      <polyline points="8 6 2 12 8 18" />
      <line x1="14" y1="4" x2="10" y2="20" />
    </svg>
  ),
  react: (
    <svg viewBox="-11.5 -10.232 23 20.463" fill="currentColor">
      <circle r="2.05" />
      <g fill="none" stroke="currentColor" strokeWidth="1">
        <ellipse rx="11" ry="4.2" />
        <ellipse rx="11" ry="4.2" transform="rotate(60)" />
        <ellipse rx="11" ry="4.2" transform="rotate(120)" />
      </g>
    </svg>
  ),
  lint: (
    <svg
      viewBox="0 0 24 24"
      fill="none"
      stroke="currentColor"
      strokeWidth="1.5"
      strokeLinecap="round"
      strokeLinejoin="round">
      <path d="M12 22s8-4 8-10V5l-8-3-8 3v7c0 6 8 10 8 10z" />
      <path d="M9 12l2 2 4-4" />
    </svg>
  ),
  tool: (
    <svg
      viewBox="0 0 24 24"
      fill="none"
      stroke="currentColor"
      strokeWidth="1.5"
      strokeLinecap="round"
      strokeLinejoin="round">
      <path d="M14.7 6.3a1 1 0 0 0 0 1.4l1.6 1.6a1 1 0 0 0 1.4 0l3.77-3.77a6 6 0 0 1-7.94 7.94l-6.91 6.91a2.12 2.12 0 0 1-3-3l6.91-6.91a6 6 0 0 1 7.94-7.94l-3.76 3.76z" />
    </svg>
  ),
  book: (
    <svg
      viewBox="0 0 24 24"
      fill="none"
      stroke="currentColor"
      strokeWidth="1.5"
      strokeLinecap="round"
      strokeLinejoin="round">
      <path d="M2 3h6a4 4 0 0 1 4 4v14a3 3 0 0 0-3-3H2z" />
      <path d="M22 3h-6a4 4 0 0 0-4 4v14a3 3 0 0 1 3-3h7z" />
    </svg>
  ),
};

const SECTIONS = [
  {
    title: 'Get Started',
    href: '/en/docs/getting-started',
    description:
      'Never used a type system before or just new to Flow? Get up and running in a few minutes.',
    icon: icons.rocket,
    color: '#e8bd36',
    hero: true,
  },
  {
    title: 'FAQ',
    href: '/en/docs/faq',
    description: 'Have a question about using Flow? Check here first!',
    icon: icons.help,
    color: '#9cd732',
  },
  {
    title: 'Language Guide',
    href: '/en/docs/types',
    description:
      'Types, narrowing, generics, enums, match expressions, and the full type system.',
    icon: icons.code,
    color: '#4a90d9',
  },
  {
    title: 'React',
    href: '/en/docs/react',
    description:
      'Component syntax, hooks, render types, events, refs, and advanced React patterns.',
    icon: icons.react,
    color: '#61dafb',
  },
  {
    title: 'Linting',
    href: '/en/docs/linting',
    description:
      'Configure lint rules, flowlint comments, and strict import/export checks.',
    icon: icons.lint,
    color: '#f5a623',
  },
  {
    title: 'Tooling',
    href: '/en/docs/editors',
    description:
      'Editor integration, Babel, Prettier, ESLint, and flow-remove-types.',
    icon: icons.tool,
    color: '#b8e986',
  },
  {
    title: 'Reference',
    href: '/en/docs/config',
    description:
      '.flowconfig options, CLI commands, Flow Strict, and library definitions.',
    icon: icons.book,
    color: '#bd93f9',
  },
];

export default function DocsLandingPage(): React.Node {
  const hero = SECTIONS.find(s => s.hero);
  const rest = SECTIONS.filter(s => !s.hero);

  return (
    <div className={styles.container}>
      {hero && (
        <Link
          href={hero.href}
          className={styles.heroCard}
          style={{'--accent': hero.color}}>
          <span className={styles.heroIcon}>{hero.icon}</span>
          <div>
            <h2 className={styles.heroTitle}>{hero.title}</h2>
            <p className={styles.heroDescription}>{hero.description}</p>
          </div>
        </Link>
      )}
      <div className={styles.grid}>
        {rest.map(section => (
          <Link
            key={section.title}
            href={section.href}
            className={styles.card}
            style={{'--accent': section.color}}>
            <span className={styles.cardIcon}>{section.icon}</span>
            <h3 className={styles.cardTitle}>{section.title}</h3>
            <p className={styles.cardDescription}>{section.description}</p>
          </Link>
        ))}
      </div>
    </div>
  );
}
