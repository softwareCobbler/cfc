<cfcomponent>
    <cfscript>
        function foo() {
            return 42;
        }

        function bar() {
            f|<<<<
        }
    </cfscript>
</cfcomponent>