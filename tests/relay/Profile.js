/**
 * @format
 * @flow
 */

import * as React from 'react';
import {
  type Fragment,
  createFragmentContainer,
  type GetPropFragmentRef,
} from './Relay';
import ProfilePic from './ProfilePic';

import type {Profile_user} from './Profile.graphql';

type Props = {
  user: Profile_user,
  foo: number,
};

class Profile extends React.Component<Props> {
  render(): React.Node {
    this.props.foo as empty; // Error: number ~> empty
    this.props.user.id as empty; // Error: string ~> empty
    this.props.user.name as empty; // Error: string ~> empty
    this.props.user.pic.id as empty; // Error: string ~> empty
    <ProfilePic image={{url: 'https://facebook.com'}} />; // Error: object ~> opaque type
    return (
      <div>
        <p>{this.props.user.name}</p>
        <p>{this.props.foo}</p>
        <p>{this.props.user.pic.id}</p>
        <ProfilePic image={this.props.user.pic} />
      </div>
    );
  }
}

export default createFragmentContainer(Profile) as component(
  ref?: React.RefSetter<Profile>,
  ...GetPropFragmentRef<Props>
);
