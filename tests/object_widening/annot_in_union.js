//@flow
// We shouldn't fire the AnnotT special case unless there
// is no more work to be done in the spread.
type LiterallyAnyObject = {|
  thisFieldIsMissing: true,
|};                     

type WrapperType = {|
  object: LiterallyAnyObject | null | LiterallyAnyObject,
|}

const prevPagers: WrapperType = {
  object: null
};

const x: LiterallyAnyObject = {
  ...prevPagers.object, // Error
};
