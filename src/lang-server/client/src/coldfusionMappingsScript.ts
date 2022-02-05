export const coldfusionMappingsScript = `
<cfscript>
	//
    // copy and paste this to some server path you want the mappings for
    //
	// this will dump out the serialized JSON of cf mappings and wirebox mappings
	// in the format expected by cfls
	// click the <pre> element that gets rendered in the browser to copy the resulting text
	// then save that text in \${editorWorkspaceRoot}/cfconfig.json
	//

	function getCfMappings() {
		var cfMappings = getApplicationMetadata().mappings;
		var result = {};
		var appRoot = expandPath("");

		for (var key in cfMappings) {
			var stripLeadingPathSep = reReplace(regex="^[\\\\/]", scope="one", string=key, substring="");
			var replacePathSepsWithDots = reReplace(regex="[\\\\/]", scope="all", string=stripLeadingPathSep, substring=".");
			result[replacePathSepsWithDots] = replace(string=cfMappings[key], scope="one", substring1=appRoot, substring2="");
		}
		return result;
	}

	// pass in a ref to your wirebox instance
	function getWireboxMappings(wirebox) {
		var mappings = wirebox.getBinder().getMappings();
		var result = {};
		for (var name in mappings.keyArray()) {
			var path = mappings[name].getPath();
			if (isSimpleValue(path)) { // sometimes it's a function?
				result[name] = path;
			}
		}
		return result;
	}

	writeoutput("
		<pre id='mappings-text' style='word-break:break-all; white-space: break-spaces;'>
		</pre>
		<script>
			const pre = document.querySelector('##mappings-text');
			const mappings = \`#serializeJSON({
				"cf": getCfMappings(),
				"wirebox": getWireboxMappings(wirebox)
			})#\`;
			pre.innerText = mappings;
			pre.addEventListener('click', () => {
				console.log('click');
				navigator.clipboard.writeText(mappings);
			})
		</script>
	")
</cfscript>`;
