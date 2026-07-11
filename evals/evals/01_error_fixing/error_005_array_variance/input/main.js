/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 */

type MediaItem = {
  id: string,
  url: string,
};

type VideoItem = {
  id: string,
  url: string,
  duration: number,
  codec: string,
};

function renderGallery(items: Array<MediaItem>): void {
  for (const item of items) {
    console.log(item.id + ': ' + item.url);
  }
}

function addPlaceholder(items: Array<MediaItem>, placeholder: MediaItem): void {
  items.push(placeholder);
}

function main(): void {
  const videos: Array<VideoItem> = [
    {id: 'v1', url: '/video1.mp4', duration: 120, codec: 'h264'},
    {id: 'v2', url: '/video2.mp4', duration: 300, codec: 'h265'},
  ];

  renderGallery(videos);

  addPlaceholder(videos, {id: 'placeholder', url: '/placeholder.png'});

  console.log('videos: ' + videos.length);
}
