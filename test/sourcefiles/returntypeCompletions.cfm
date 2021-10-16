//// @file="/RootSibling.cfc"
component {
    public function someRootSiblingMethod() {}
}

//// @file="/Top.cfc"
component {
    public RootSibling function shouldReturnRootSibling() {}
}

//// @file="/Base.cfc"
component extends="Top" {
    public function someBaseMethod() {}
}

//// @file="/foo/Child.cfc"
component extends="Base" {
    public function someChildMethod() {
        shouldReturnRootSibling().|<<<<
    }
    private function shouldBeNotExportedBecauseItIsPrivate() {}
}

//// @file="/foo/Child2.cfc"
component extends="Base" {
    public function someChildMethod() {
        so|<<<< // should get someChildMethod, shouldBeNotExportedBecauseItIsPrivate, Base::someBaseMethod, Top::shouldReturnRootSibling
    }
    private function shouldBeNotExportedBecauseItIsPrivate() {}
}

//// @file="/foo/Impl.cfm"
<cfscript>
    new Child().|<<<< // should get Child::someChildMethod, Child::Base::someBaseMethod, Child::Base::Top::shouldReturnRootSibling
</cfscript>