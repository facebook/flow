---
title: Multi-platform Support for React Native
slug: /react/multiplatform
description: "Flow's support for multiple platforms inside a single React Native codebase"
---

:::caution
The feature is still experimental. Behaviors might change in the future.
:::

## Benefits {#toc-benefits}

React Native supports conditional bundling of files with [platform specific extensions](https://reactnative.dev/docs/platform-specific-code#platform-specific-extensions). For example, if you have different implementations of an Image component for iOS and Android, you can have an `Image.ios.js` file and `Image.android.js` file, and an import of Image can be resolved to either file based on the platform you are targeting.

These platform specific files live under the same repository, but it would normally require two flowconfigs to check them like the following setup:

```toml title=.flowconfig
; for ios
[ignore]
.*\.android\.js$
[options]
module.file_ext=.js
module.file_ext=.ios.js
```

```toml title=.flowconfig.android
; for android
[ignore]
; Ignore other platform suffixes
.*\.ios\.js$
[options]
module.file_ext=.js
module.file_ext=.android.js
```

Flow's optional React Native multi-platform support allows you to check your entire project with mixed platforms under a single Flow root, so that during the development of a module with both .ios and .android files, you no longer have to run both Flow servers and constantly switch between different servers to see type errors on different platforms.

## Quick Start {#toc-quick-start}

You can start by deleting the flowconfig for all other platforms, deleting all the platform specific configs in the only remaining flowconfig, and add the following new lines to the `options` section:

```
experimental.multi_platform=true
experimental.multi_platform.extensions=.ios
experimental.multi_platform.extensions=.android
```

For example, these are the required changes for the `.flowconfig` example above:

```diff title=.flowconfig
[ignore]
- .*\.android\.js$
[options]
module.file_ext=.js
- module.file_ext=.ios.js
+ experimental.multi_platform=true
+ experimental.multi_platform.extensions=.ios
+ experimental.multi_platform.extensions=.android
```

After enabling the new configurations, there will likely be new errors. The sections below explain the additional rules that Flow imposes to check a multiplatform React Native project.

## Common Interface Files {#toc-common-interface-file}

Suppose you have a file that imports the `Image` module, but `Image` module has different iOS and Android implementations as follows:

```jsx title=MyReactNativeApp.js
import * as React from 'react';
import Image from './Image';

<Image src="/hello.png" />;
<Image src="/world.png" lazyLoading={true} />;
```

```jsx title=Image.ios.js
import * as React from 'react';

type Props = { src: string, lazyLoading?: boolean };

export default function Image(props: Props): React.Node { /* ... */ }
```

```jsx title=Image.android.js
import * as React from 'react';

type Props = { src: string, lazyLoading: boolean };

export default class Image extends React.Components<Props> {
  static defaultProps: { lazyLoading: boolean } = { lazyLoading: false };
  render(): React.Node { /* ... */ }
}
```

When you enabled multiplatform support, you will likely see that error that the `./Image` module cannot be resolved. To fix the error, you need to create a common interface file under the same directory:

### Common Interface File in `.js.flow` {#toc-common-interface-file-in-js-flow}

One option is to write a common interface file in `.js.flow`:

With [Component Types](../component-types/)
```jsx title=Image.js.flow
import * as React from 'react';

type Props = { src: string, lazyLoading?: boolean };

declare const Image: component(...Props);
export default Image;
```

With [React.ComponentType](../types#toc-react-componenttype)
```jsx title=Image.js.flow
import * as React from 'react';

type Props = { src: string, lazyLoading?: boolean };

declare const Image: React.ComponentType<Props>;
export default Image;
```

Flow will ensure that the module types of both `Image.ios.js` and `./Image.android.js` are subtype of the module type of `./Image.js.flow`. Flow will also ensure that there exists an implementation for each platform you declared in your `.flowconfig`.

### Common Interface File in `.js` {#toc-common-interface-file-in-js}

Sometimes you might target desktop platforms in addition to iOS and Android, and you only have a special implementation for one platform, and all the other platforms will use the fallback implementation in a `.js` file. For example:

```jsx title=Image.js
import * as React from 'react';
import DefaultImage from 'react-native/Libraries/Image';

export default DefaultImage;
```

```jsx title=Image.ios.js
import * as React from 'react';

type Props = { src: string, lazyLoading: boolean };

export default function Image(props: Props): React.Node {
  // Custom implementation to take advantage of some unique iOS capabilities
}
```

In this case, Flow will use the `.js` file as the common interface file, and check all other platform-specific implementation files' against the `.js` file. Since the `.js` file is already a fallback implementation, Flow will no longer require that platform-specific implementation files exist for all platforms.
