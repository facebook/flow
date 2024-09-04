import {foo} from 'mobile' // error
import '../windows-only/windows-specific' // error, cannot import windows-only file with windows extension from desktop (mac, windows) file
import '../windows-only/general' // error, cannot import windows-only file from desktop (mac, windows) file
