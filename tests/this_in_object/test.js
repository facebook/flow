// @flow

let a = {
    m() {
        this; // error, refer to method on line 4

        let b = { m() { this }}; // error, refer to method here

        let c = class {
            m() {
                this; // fine

                let d = { m() { this }}; // error, refer to method here
            }
        }

        let e = () => { this }; // error, refer to method on line 4

        function f() {
            this; // fine

            let g = { m() { this }}; // error, refer to method here
        }

        let h = [this]; // error, refer to method on line 4
    },

    f : function () {
        this; // fine

        let b = { m() { this }}; // error, refer to method here

        let c = class {
            m() {
                this; // fine

                let d = { m() { this }}; // error, refer to method here
            }
        }

        let e = () => { this }; // fine

        function f() {
            this; // fine

            let g = { m() { this }}; // error, refer to method here
        }

        let h = [this]; // fine
    },

    a : () => {
        this; // fine

        let b = { m() { this }}; // error, refer to method here

        let c = class {
            m() {
                this; // fine

                let d = { m() { this }}; // error, refer to method here
            }
        }

        let e = () => { this }; // fine

        function f() {
            this; // fine

            let g = { m() { this }}; // error, refer to method here
        }

        let h = [this]; // fine
    }
}

let x = {
  m() {
    let x = { f : this }; // error, point to m
    let y = ({ f() { this }}) // error, point to f
    let z = ({ f() { let z = { f: this } }}) // error, point to f
  }
}
