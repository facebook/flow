jest.createMockFromModule('./module') // ok
jest.mock('./module') // ok
jest.unmock('./module') // ok
jest.deepUnmock('./module') // ok
jest.doMock('./module') // ok
jest.dontMock('./module') // ok
jest.setMock('./module') // ok
jest.requireActual('./module') // ok
jest.requireMock('./module') // ok

jest.createMockFromModule('BAD') // error
jest.mock('BAD') // error
jest.unmock('BAD') // error
jest.deepUnmock('BAD') // error
jest.doMock('BAD')
jest.dontMock('BAD') // error
jest.setMock('BAD') // error
jest.requireActual('BAD') // error
jest.requireMock('BAD') // error
