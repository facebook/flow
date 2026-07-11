// @flow

export function startSession(userName: string): string {
  track('session_start', {userId: userName});
  return `${Analytics.appId}:${userName}`;
}
