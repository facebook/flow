// This project models the setup where we have common code shared by web and native,
// but the flowconfig only includes web + common code.
// In this case, it's helpful to assume that the common code only has web platform.

// Under experimental.projects.strict_boundary=true, it will be an error.
// With experimental.projects.strict_boundary=false,
// we permissively assume the common code only has the platform of the first project (in this case web)
import foo from '../web/web'; // ok
