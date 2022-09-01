// @flow

const mediaSource = new MediaSource();

// properties
const sourceBuffers: SourceBufferList = mediaSource.sourceBuffers // valid
const activeSourceBuffers: SourceBufferList = mediaSource.activeSourceBuffers // valid
const readyState: "closed"|"open"|"ended" = mediaSource.readyState // valid
const duration: number = mediaSource.duration // valid
const handle: MediaSourceHandle = mediaSource.handle // valid

// methods
const sourceBuffer: SourceBuffer = mediaSource.addSourceBuffer("hello") // valid
mediaSource.removeSourceBuffer(sourceBuffer);
mediaSource.endOfStream("hello") // valid
const isSupported = MediaSource.isTypeSupported("hello"); // valid
