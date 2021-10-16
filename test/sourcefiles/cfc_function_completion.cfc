<cfcomponent>
    <cfscript>
        function foo() {
            return 42;
        }

        function bar() {
            f|<<<<   // at the moment, completions will return `foo` and `bar` from this position, and the client filters `bar` away
        }
    </cfscript>
</cfcomponent>