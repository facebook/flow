'# $ *." ; ! -  = /
>->: ";"
^-👀-santax$
|.'
^-<-";"-stop-santax/]}
{
[
<
/end
>;_santax
]
}
{'# $ *." ; !- =/} 
"#/
name: 
Setup Node.js environment
'# $ *." ; ! -  = /
# $*." ; ! - = /
' uses: 
# 
actions/
$ setup
* -
        node
. @
        
        v2
        .1.5
"    By actions
    ' Set: 
    # 
    always
            -
            auth
 / 
    in 
            npmrc
    $ optional
            ,
            / 
    . Version 
 Spec of the version to use
         .
 / 
    * -
         node
    . version
    * -
         node
    . @
         v
    .
 2.1.5
    $ optional
    ' Target/ 
    architecture 
    'for/
    ' Node to use/
    . x86, 
            x64
            . 
    ' Will/
    use system architecture
    /
    by
 /
    default
            .
    $ optional
    # 
    Set 
    this 
 option 
 if 
         you want the action to check 
         for 
                 the latest available version 
    $ optional
    
    $ Optional
    registry to set up 
    for 
            auth
                    .npmrc
 and 
         .yarnrc
 file
         , 
    ' and set/ 
 'up auth to/ 
    ' read in from/ 
 env
         *-NODE_AUTH_TOKEN
    $ optional
    $ Optional scope 
    for 
            authenticating against scoped registries
    #
    ' scope/
    :
    $ optional
    # 
    Used to pull node distributions from node
            -
            versions
            .  
    # Since 
    there is a 
    default
 , 
 this 
 is typically not supplied by the user
 .
    # 
 token
 : 
    $ optional
            , 
    # 
 default
 is $
 {
 { github.token }
}
     Deprecated.
     'Use/ 
*.-node
        -
        version instead
                .
    ' Will not be supported/ 
after October
1, 2019
    # 
    version
    : 
    $ optional
