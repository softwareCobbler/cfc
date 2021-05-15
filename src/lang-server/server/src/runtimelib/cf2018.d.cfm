<!--- in a declaration file, comments are tag comments, and they nest just as they do in <!--- cfm files ---> --->
<!--- type annotations are placed in comments so that it works in CFMs/CFCs without having to compile/emit --->

@declare function queryFilter(
    required callback /*: (required row: number, currentRow: number, query: query<any>) => void*/,
    parallel /*: {v: number, u: string}[]*/ = 42,
    maxThreadCount /*: number*/) /*: query<any>*/;

<!---
@declare global cgi /*: {
    auth_password: string,
    auth_type: string,
    auth_user: string,
    cert_cookie: string,
    cert_flags: string,
    cert_issuer: string,
    cert_keysize: string,
    cert_secretkeysize: string,
    cert_serialnumber: string,
    cert_server_issuer: string,
    cert_server_subject: string,
    cert_subject: string,
    cf_template_path: string,
    content_length: string,
    content_type: string,
    context_path: string,
    gateway_interface: string,
    https: string,
    https_keysize: string,
    https_secretkeysize: string,
    https_server_issuer: string,
    https_server_subject: string,
    http_accept: string,
    http_accept_encoding: string,
    http_accept_language: string,
    http_connection: string,
    http_cookie: string,
    http_host: string,
    http_referer: string,
    http_user_agent: string,
    query_string: string,
    remote_addr: string,
    remote_host: string,
    remote_user: string,
    request_method: string,
    script_name: string,
    server_name: string,
    server_port: string,
    server_port_secure: string,
    server_protocol: string,
    server_software: string,
}*/;
--->