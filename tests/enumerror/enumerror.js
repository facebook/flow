/** @flow */

/*
var MyStates = {
    PAUSED: 'PAUSED',
    ACTIVE: 'ACTIVE',
    DELETED: 'DELETED',
};
function isActive(ad: {state: $Enum<$typeof<MyStates> >}): boolean {
    return ad.state === MyStates.ACTIVE;
};
*/

function isActive(ad: {state: $Enum<{
    PAUSED: string;
    ACTIVE: string;
    DELETED: string;
}>}): boolean {
    return ad.state === 'ACTIVE';
};
isActive({state: 'PAUSE'});
