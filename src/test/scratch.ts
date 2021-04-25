// quick scratch debug;
// throw some text into the scanner,
// set the parser to either CFM/CFC mode,
// rebuild and then run the debugger
import { Scanner, TokenizerMode, Parser } from "../compiler";
import { CfFileType } from "../compiler/parser";

//const scanner = Scanner(`<cfset x = function foo(a, b = 42 & 0){}>`);
const scanner = Scanner(`
<cfcomponent output=#false#>
	<cffunction name="toStructs" returnType="struct" hint="Given a TestResult XML string, converts it to a structure. Obsolete?">
		<cfargument name="xml" required="true" type="String">
		<cfset var s = StructNew()>
		<cfset var xmltext = trim(arguments.xml)>
		<cfset var xmldoc = "">
		<cfset final var n = "">
		<cfset var a_tests = ArrayNew(1)>
		<cfset var componentName = "">
		<cfset testName = "">
		<cfset var tc = ArrayNew(1)>
		<cfset var child = 1>
		<cfset xmldoc = XMLParse(xmltext)>
		<cfset a_tests = xmlsearch(xmldoc,"test_results/test_case")>
		
		<cfloop from="1" to="#ArrayLen(a_tests)#" index="i">
			<cfset componentName = a_tests[i].xmlattributes.component>
			<cfset testName = a_tests[i].xmlattributes.testname>
			<cfif not StructKeyExists(s,componentName)>
				<cfset s[componentName] = StructNew()>
			</cfif>
			<cfset s[componentName][testName] = StructNew()>
			<cfset s[componentName][testName].result = a_tests[i].results.message.xmltext>			
			<cfset s[componentName][testName].message = a_tests[i].results.details.xmltext>
			<cfif StructKeyExists(a_tests[i],"trace")>
				<cfset s[componentName][testName].output = a_tests[i].trace.message.xmltext>
			<cfelse>
				<cfset s[componentName][testName].output = "">
			</cfif>
			
			<!--- add any exceptions --->
			<cfif StructKeyExists(a_tests[i].results,"tagcontext")>
				<cfset tc = ArrayNew(1)>
				<!--- <cfdump var="#a_tests[i].results.tagcontext.xmlchildren#"><cfabort> --->
				<cfloop from="1" to="#ArrayLen(a_tests[i].results.tagcontext.xmlchildren)#" index="child">
					<cfset tc[child] = StructNew()>
					<cfset tc[child].file = a_tests[i].results.tagcontext.xmlchildren[child].file.xmltext>
					<cfset tc[child].line = a_tests[i].results.tagcontext.xmlchildren[child].line.xmltext>
				</cfloop>
				<cfset s[componentName][testName].tagcontext = tc>
				<cfset s[componentName][testName].exception = a_tests[i].results.exception.xmltext>
			</cfif>
			
		</cfloop>
		
		<cfreturn s>
	</cffunction>
	
	<cfif z ?: 3>
		<!---<cfscript>
			final x = 4;
			final var y = 3 + ;
			if (!!v) {
				var f = function foo() {
					x = {a:1, b: 2,};
				}
			}
		</cfscript>

		<cftransaction />

		</cftransaction>--->
		<cfoutput
	</cfif>

</cfcomponent>



`);

const parser = Parser()
    .setScanner(scanner)
    .setDebug(true);

parser.parse(CfFileType.cfm);
    
for (const diag of parser.getDiagnostics()) {
    console.log(diag);
}