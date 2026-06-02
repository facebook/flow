/**
 * @format
 * @flow
 */

import * as Relay from './Relay';
import type {ProfilePic_imageRef} from './ProfilePic.graphql';

declare export opaque type Profile_userRef;

export type Profile_user = Relay.Fragment<
  Profile_userRef,
  {
    readonly $$typeof: Relay.$$TypeofFragment,
    readonly id: string,
    readonly name: string,
    readonly pic: ProfilePic_imageRef & {
      readonly id: string,
    },
  },
>;
