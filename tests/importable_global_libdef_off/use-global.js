// Without the flag, a global libdef still contributes to global scope.
const ok: string = MyGlobal;
const bad: number = MyGlobal; // error

module.exports = {ok, bad};
