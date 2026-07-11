/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import * as React from 'react';
import {useState} from 'react';

type Filters = {category: string, minPrice: number, tags: Array<string>};

hook useFilters(): {
  filters: Filters,
  setCategory: (category: string) => void,
  addTag: (tag: string) => void,
} {
  const [filters, setFilters] = useState<Filters>({
    category: 'all',
    minPrice: 0,
    tags: [],
  });

  const setCategory = (category: string) => {
    filters.category = category;
    setFilters(filters);
  };

  const addTag = (tag: string) => {
    filters.tags.push(tag);
    setFilters(filters);
  };

  return {filters, setCategory, addTag};
}

component FilterPanel() {
  const {filters, setCategory, addTag} = useFilters();
  return (
    <div>
      <span>{filters.category}</span>
      <button onClick={() => setCategory('books')}>Books</button>
      <button onClick={() => addTag('sale')}>Add Sale Tag</button>
    </div>
  );
}
