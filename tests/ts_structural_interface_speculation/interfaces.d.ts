interface ResolvedLower {}

interface RequiredBase {
  baseFirst: string;
  baseSecond: number;
}

interface RequiredMixin {
  mixinFirst: string;
  mixinSecond: number;
}

interface RequiredInterface extends RequiredBase, RequiredMixin {
  ownFirst: string;
  ownSecond: number;
}

interface IncompatibleLower {
  baseFirst: number;
  baseSecond: string;
  mixinFirst: number;
  mixinSecond: string;
  ownFirst: number;
  ownSecond: string;
}

interface CompatibleLower extends RequiredInterface {
  extra: boolean;
}

declare const resolvedLower: ResolvedLower;
declare const incompatibleLower: IncompatibleLower;
declare const compatibleLower: CompatibleLower;
