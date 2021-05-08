<cfcomponent>
    <cffunction name="hmmmm">
        <cfscript>
        </cfscript>
    </cffunction>

    <cffunction name="foo">
        <cfargument name="lel">
        <cfquery name="local.q">
            <cfscript>
            </cfscript>
        </cfquery>
        <cfreturn {ok:true}>
    </cffunction>

    <cftry>
        <cfcatch>
        </cfcatch>
        <cffinally>
            <cftry>
                <cfcatch>
                    <cfswitch>
                        <cfcase value=#x#>
                            <cfscript>
                                function foo(haha, ok) {
                                    arguments.|<<<<
                                    for (var x = 0; x < 10; x++) {

                                    }
                                }
                            </cfscript>
                        </cfcase>
                    </cfswitch>
                </cfcatch>
            </cftry>
        </cffinally>
    </cftry>
</cfcomponent>