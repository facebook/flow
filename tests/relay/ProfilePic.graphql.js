/**
 * @format
 * @flow
 */

import * as Relay from './Relay';

declare export opaque type ProfilePic_imageRef;

export type ProfilePic_image = Relay.Fragment<
  ProfilePic_imageRef,
  {
    readonly $$typeof: Relay.$$TypeofFragment,
    readonly url: string,
  },
>;
