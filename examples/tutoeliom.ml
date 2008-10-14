(*zap*
   this is the Eliom Tutorial.
   It is mainly written in html.
   You can find a more readable version of comments on http://www.ocsigen.org
*zap*)
(*zap*
~/bin/caml2html -css -hc2 -oc tutoeliom.ml
*zap*)
(*html*

let part0 sp =
°:xmllist°

    <div class="onecol">
      <p>This is the programmer's guide
        for <em>Eliom</em> (development version).
        Please report any error in this tutorial
        and send us your comments and suggestions!
      </p>
      <p>This manual is also available as a <a href="eliom-1.1.0.pdf">pdf file</a>.</p>
      <p>Eliom is an extension for the Web server <em>Ocsigen</em>
         that allows dynamic webpages generation.
         It uses very new concepts making programming very different
         from all other Web programming tools.
         It allows to write a complex Web site in very few lines of code.
      </p>
      <p><em>Warning: This tutorial assumes you know the
        <em>Objective Caml</em> language.</em></p>
    </div>

°°



let part1 sp =
°:xmllist°


   <h2>1. The basics: main services, parameters, forms, cooperative programming</h2>
    <h3 id="p1baseprinciples">Base principles</h3>
    <div class="onecol">
      <p>Unlike many other Web programming techniques (CGI, PHP,&nbsp;...),
          with Eliom, you don't write one file for each URL, but
          a caml module (cmo or cma) for the whole Web site.</p>
      <p>
          The $a ~service:senddoc ~sp [code [pcdata "Eliom_services" ]] [version;"Eliom_services.html"]$ module allows to create new entry points to
          your Web site, called <em>services</em>. In general, services are
          attached to an URL and generate a Web page.
          They are represented by OCaml values, on which
          you must register a function that will generate a page.
          There are several ways to create pages for Eliom. This tutorial
          is mainly using $a ~service:senddoc ~sp [code [pcdata "Eliom_predefmod.Xhtml" ]] [version;"Eliom_predefmod.Xhtml.html"]$, a module allowing
          to register xhtml pages statically typed using OCaml's
          polymorphic variants.
  The $a ~service:senddoc ~sp [code [pcdata "XHTML.M" ]] [version;"XHTML.M.html"]$ module defines functions to construct
          xhtml pages using that type system.
          As the $a ~service:senddoc ~sp [code [pcdata "Eliom_predefmod.Xhtml" ]] [version;"Eliom_predefmod.Xhtml.html"]$ redefines some functions
          of $a ~service:senddoc ~sp [code [pcdata "XHTML.M" ]] [version;"XHTML.M.html"]$, open the modules in this order:
      </p>
*html*)
open Lwt
open XHTML.M
open Eliom_services
open Eliom_parameters
open Eliom_sessions
open Eliom_predefmod.Xhtml
(*html*
      <p>$a ~service:senddoc ~sp [code [pcdata "Lwt" ]] [version;"Lwt.html"]$
      (lightweight threads) is the cooperative thread library used by Ocsigen
      (<a href="#p1threads">see later</a>).</p>
      <p>Here is an example showing how to create a new service and
         register a page created with XHTML.M. Use the function
         $a ~fragment:"VALregister_new_service" ~service:senddoc ~sp [code [pcdata "Eliom_predefmod.Xhtml.register_new_service" ]] [version;"Eliom_mkreg.ELIOMREGSIG1.html"]$:
      </p>
*html*)
let coucou =
  register_new_service
    ~path:["coucou"]
    ~get_params:unit
    (fun _ () () ->
      return
        (html
           (head (title (pcdata "")) [])
           (body [h1 [pcdata "Hallo!"]])))
(*html*
The same, written with fully qualified values:
<pre>
<span class="Clet">let</span> coucou <span class="Cnonalphakeyword">=</span>
  <span class="Cconstructor">Eliom_predefmod</span><span class="Cnonalphakeyword">.</span><span class="Cconstructor">Xhtml</span><span class="Cnonalphakeyword">.</span>register_new_service
    <span class="Clabel">~path:</span><span class="Cnonalphakeyword">[</span><span class="Cstring">"coucou"</span><span class="Cnonalphakeyword">]</span>
    <span class="Clabel">~get_params:</span><span class="Cconstructor">Eliom_parameters</span><span class="Cnonalphakeyword">.</span>unit
    <span class="Cnonalphakeyword">(</span><span class="Cfun">fun</span> <span class="Cnonalphakeyword">_</span> <span class="Cnonalphakeyword">(</span><span class="Cnonalphakeyword">)</span> <span class="Cnonalphakeyword">(</span><span class="Cnonalphakeyword">)</span> <span class="Cnonalphakeyword">-&gt;</span>
      <span class="Cconstructor">Lwt</span><span class="Cnonalphakeyword">.</span>return
        <span class="Cnonalphakeyword">(</span><span class="Cconstructor">XHTML</span><span class="Cnonalphakeyword">.</span><span class="Cconstructor">M</span><span class="Cnonalphakeyword">.</span>html
          <span class="Cnonalphakeyword">(</span><span class="Cconstructor">XHTML</span><span class="Cnonalphakeyword">.</span><span class="Cconstructor">M</span><span class="Cnonalphakeyword">.</span>head <span class="Cnonalphakeyword">(</span><span class="Cconstructor">XHTML</span><span class="Cnonalphakeyword">.</span><span class="Cconstructor">M</span><span class="Cnonalphakeyword">.</span>title <span class="Cnonalphakeyword">(</span><span class="Cconstructor">XHTML</span><span class="Cnonalphakeyword">.</span><span class="Cconstructor">M</span><span class="Cnonalphakeyword">.</span>pcdata <span class="Cstring">""</span><span class="Cnonalphakeyword">)</span><span class="Cnonalphakeyword">)</span> <span class="Cnonalphakeyword">[</span><span class="Cnonalphakeyword">]</span><span class="Cnonalphakeyword">)</span>
          <span class="Cnonalphakeyword">(</span><span class="Cconstructor">XHTML</span><span class="Cnonalphakeyword">.</span><span class="Cconstructor">M</span><span class="Cnonalphakeyword">.</span>body <span class="Cnonalphakeyword">[</span><span class="Cconstructor">XHTML</span><span class="Cnonalphakeyword">.</span><span class="Cconstructor">M</span><span class="Cnonalphakeyword">.</span>h1 <span class="Cnonalphakeyword">[</span><span class="Cconstructor">XHTML</span><span class="Cnonalphakeyword">.</span><span class="Cconstructor">M</span><span class="Cnonalphakeyword">.</span>pcdata <span class="Cstring">"Hallo!"</span><span class="Cnonalphakeyword">]</span><span class="Cnonalphakeyword">]</span><span class="Cnonalphakeyword">)</span><span class="Cnonalphakeyword">)</span><span class="Cnonalphakeyword">)</span>
</pre>
*html*)(*zap*
let coucou =
  Eliom_predefmod.Xhtml.register_new_service
    ~path:["coucou"]
    ~get_params:Eliom_parameters.unit
    (fun _ () () ->
      Lwt.return
        (XHTML.M.html
          (XHTML.M.head (XHTML.M.title (XHTML.M.pcdata "")) [])
          (XHTML.M.body [XHTML.M.h1 [XHTML.M.pcdata "Hallo!"]])))
*zap*)(*html*
      <p>As you can see,
      $a ~fragment:"VALreturn" ~service:senddoc ~sp [code [pcdata "return" ]] [version;"Lwt.html"]$ is a function from $a ~service:senddoc ~sp [code [pcdata "Lwt" ]] [version;"Lwt.html"]$.
      Use it like this for instants, and
      <a href="#p1threads">see later</a> for more advanced use.</p>
      <p>
      Now you can compile your file (here <code>tutorial.ml</code>)
        by typing :</p>
      <pre>ocamlc -I /<em>path_to</em>/ocsigen/ -I /<em>path_to</em>/lwt/ -c tutorial.ml</pre>
      <p>If you use findlib, you can also use the following command line:</p>
      <pre>ocamlfind ocamlc -thread -package ocsigen -c tutorial.ml</pre>
      <p>
      Replace <code>/<em>path_to</em>/ocsigen/</code>
       by the directory where Ocsigen libraries are installed (that contains
       <code>eliom.cma</code>, <code>staticmod.cmo</code>, etc.),
       usually something like
      <code>/usr/lib/ocaml/3.09.3/ocsigen</code> or
      <code>/usr/local/lib/ocaml/3.09.3/ocsigen</code> or
      <code>/opt/godi/lib/ocaml/site-lib/ocsigen</code>.
      </p>
      <p>
      Add the following lines to Ocsigen's config file
      (<code>/etc/ocsigen/ocsigen.conf</code> most of the time):
      </p>
      <pre>&lt;host&gt;
 &lt;site path="examples"&gt;
  &lt;eliom module="/<em>path_to</em>/tutoeliom.cmo" /&gt;
 &lt;/site&gt;
&lt;/host&gt;</pre>
      <p>Note that if your module has a findlib <code>META</code> file,
        it is also possible to do:</p>
      <pre>&lt;host&gt;
 &lt;site path="examples"&gt;
  &lt;eliom findlib-package="<em>package-name</em>" /&gt;
 &lt;/site&gt;
&lt;/host&gt;</pre>
      <p>Then run ocsigen. You should see your page at url
           <code>http://<em>your_server</em>/examples/coucou</code>.
           See this example $a Tutoeliom.coucou sp <:xmllist< here >> ()$.
      </p>
      <p>NB: See the default config file to see how to set the port on
              which your server is running, the user who runs it, the path
            of the log files, etc.
      </p>
      <p>Here is a sample
   $a ~service:(static_dir sp) ~sp [pcdata "Makefile"] ["Makefile"]$ for your modules.</p>
      <h4>Static typing of XHTML with XHTML.M</h4>
        <p>
        Typing of xhtml with $a ~service:senddoc ~sp [code [pcdata "XHTML.M" ]] [version;"XHTML.M.html"]$ and $a ~service:senddoc ~sp [code [pcdata "Eliom_predefmod.Xhtml" ]] [version;"Eliom_predefmod.Xhtml.html"]$
        is very strict and compels you to respect
        xhtml 1.1 standard (with some limitations).
        For example if you write:
        </p>
<pre><span class="Cnonalphakeyword">(</span>html
   <span class="Cnonalphakeyword">(</span>head <span class="Cnonalphakeyword">(</span>title <span class="Cnonalphakeyword">(</span>pcdata <span class="Cstring">""</span><span class="Cnonalphakeyword">)</span><span class="Cnonalphakeyword">)</span> <span class="Cnonalphakeyword">[</span><span class="Cnonalphakeyword">]</span><span class="Cnonalphakeyword">)</span>
   <span class="Cnonalphakeyword">(</span>body <span class="Cnonalphakeyword">[</span>pcdata <span class="Cstring">"Hallo"</span><span class="Cnonalphakeyword">]</span><span class="Cnonalphakeyword">)</span><span class="Cnonalphakeyword">)</span></pre>

        <p>You will get the following error message:</p>
<pre>This expression has type ([&gt; `PCDATA ] as 'a) XHTML.M.elt
but is here used with type
([&lt; XHTML.M.block ] as 'b) XHTML.M.elt
Type 'a is not compatible with type
'b =
  [&lt; `Address | `Blockquote | `Del | `Div | `Dl | `Fieldset
   | `Form | `H1 | `H2 | `H3 | `H4 | `H5 | `H6 | `Hr | `Ins
   | `Noscript | `Ol | `P | `Pre | `Script | `Table | `Ul ]</pre>
   <p><code>'b</code> is the type of block tags (only tags allowed inside
   <code>&lt;body&gt;</code>), but PCDATA
        (i.e. raw text) is not a block tag.</p>



   <p>In XHTML, some tags cannot be empty. For example
   <code>&lt;table&gt;</code> must contain at least one row.
   To enforce this, the $a ~fragment:"VALtable" ~service:senddoc ~sp [code [pcdata "XHTML.M.table" ]] [version;"XHTML.M.html"]$ function takes two parameters:
   the first one is the first row, the second one is a list
   containing all the other rows.
   (same thing for <code>&lt;tr&gt;</code> <code>&lt;form&gt;</code>
<code>&lt;dl&gt;</code> <code>&lt;ol&gt;</code> <code>&lt;ul&gt;</code>
<code>&lt;dd&gt;</code> <code>&lt;select&gt;</code> ...)
 This forces the user to handle the empty list case specially and thus make
 the output conform to the DTD.
  </p>
   <p>
   A more detailed introduction to <code>XHTML.M</code> is available
         $a ~service:senddoc ~sp [code [pcdata "here" ]] [version;"XHTML.M.html"]
(*              ["http://theorie.physik.uni-wuerzburg.de/~ohl/xhtml/"] *)
$.
   Take a quick look at it before continuing this tutorial.
   </p>
      <div class="encadre">
        <h4>Alternate syntax</h4>
          <p>
          If you prefer using a syntax closer to html, you can write:</p>
*html*)
let coucou1 =
  Eliom_predefmod.Xhtml.register_new_service
    ~path:["coucou1"]
    ~get_params:Eliom_parameters.unit
    (fun _ () () ->
      return
        << <html>
             <head><title></title></head>
             <body><h1>Coucou</h1></body>
           </html> >>)
(*html*
      <p>To compile this syntax, you need a camlp4 syntax extension:</p>
      <pre>ocamlc -I /<em>path_to</em>/ocsigen/
 -pp "camlp4o /<em>path_to</em>/ocsigen/xhtmlsyntax.cma -loc loc"
 -c tutorial.ml</pre>
      <p>
         (Replace <code>/<em>path_to</em>/ocsigen/</code>
       by the directory where ocsigen is installed).
           See this example $a Tutoeliom.coucou1 sp <:xmllist< here >> ()$.
      </p>
      <p>
         As the syntax extension is using the same typing system as XHTML.M,
         You can mix the two syntaxes (<a href="#p1postforms">see later</a>).
      </p>
      <p>
         <em>Warning:</em> The two syntaxes are not equivalent for typing.
         Using the syntax extension will do less checking.
         For example the following code is accepted but not valid
         regarding xhtml's dtd (because <code>&lt;head&gt;</code>
         must contain a title):
      </p>
<pre>&lt;&lt; <span class="Cnonalphakeyword">&lt;</span>html<span class="Cnonalphakeyword">&gt;</span>
     <span class="Cnonalphakeyword">&lt;</span>head&gt;&lt;/head<span class="Cnonalphakeyword">&gt;</span>
     <span class="Cnonalphakeyword">&lt;</span>body&gt;&lt;h1<span class="Cnonalphakeyword">&gt;</span>plop&lt;/h1&gt;&lt;/body<span class="Cnonalphakeyword">&gt;</span>
   &lt;/html<span class="Cnonalphakeyword">&gt;</span> &gt;&gt;</pre>
      <p>
        We recommend you to use
        the functions from $a ~service:senddoc ~sp [code [pcdata "XHTML.M" ]] [version;"XHTML.M.html"]$, as you will (almost)
        always get valid xhtml.
        Use the syntax extension for example to enclose already created pieces
        of html, and check your pages validity with the
        $a (new_external_service "http://validator.w3.org" [] unit unit ())
           sp <:xmllist< W3C validator >> ()$.
      </p>
      <p>
        $a ~service:senddoc ~sp [code [pcdata "More info" ]] [version;"XHTML.M.html"]$
        on <code>XHTML.M</code>.
      </p>
      <p>
       $a xhtmlsyntax sp <:xmllist< More info >> ()$ on the syntax extension.
      </p>
      </div>
      <div class="encadre">
        <h4>Eliom and OCamlDuce</h4>
        <p>If OCamlDuce is installed on your system, it is now possible to use
        it instead of XHTML.M and Eliom_parameters.Xhtml
        to typecheck your pages. You will get a stronger type checking
        and more flexibility (easier to use other XML types, to parse
        incoming XML data, etc.).</p>
        <p>To use it, make sure that you have Eliom compiled with OCamlDuce
         support. Then dynlink <code>ocamlduce.cma</code> and
          <code>eliomduce.cma</code> from the configuration file
        (after <code>eliom.cma</code>).
        Then use $a ~service:senddoc ~sp [code [pcdata "Eliom_duce.Xhtml" ]] [version;"Eliom_duce.Xhtml.html"]$ instead of
        $a ~service:senddoc ~sp [code [pcdata "Eliom_predefmod.Xhtml" ]] [version;"Eliom_predefmod.Xhtml.html"]$ to register your pages.
        </p>
        <p>Here is an example:</p>
        <pre><span style="color:#cc9900">open</span> <span style="color:#0033cc">Lwt</span>

<span style="color:green">let</span> s =
  <span class="Cconstructor">Eliom_duce</span><span class="Cnonalphakeyword">.</span><span class="Cconstructor">Xhtml</span><span class="Cnonalphakeyword">.</span>register_new_service
    <span style="color:#770000">~path:</span>[<span style="color:#aa4444">""</span>]
    <span style="color:#770000">~get_params:</span>unit
    (<span style="color:green">fun</span> sp () () -&gt;
      return
        {{ &lt;html&gt;
             [&lt;head&gt; [&lt;title&gt; <span style="color:#aa4444">""</span>]
              &lt;body&gt; [&lt;h1&gt; <span style="color:#aa4444">"This page has been type checked by OCamlDuce"</span>]] }}) </pre>
      </div>
      <div class="encadre">
        <h4>Eliom_predefmod.HtmlText</h4>
        <p>If you want to register untyped (text) pages, use the
         functions from $a ~service:senddoc ~sp [code [pcdata "Eliom_predefmod.HtmlText" ]] [version;"Eliom_predefmod.HtmlText.html"]$, for example
         $a ~service:senddoc ~sp [code [pcdata "Eliom_predefmod.Text.register_new_service" ]] [version;"Eliom_predefmod.Text.html"]$ :
        </p>
*html*)
let coucoutext =
  Eliom_predefmod.HtmlText.register_new_service
    ~path:["coucoutext"]
    ~get_params:Eliom_parameters.unit
    (fun sp () () ->
      return
        ("<html>n'importe quoi "^
         (Eliom_predefmod.HtmlText.a coucou sp "clic" ())^
         "</html>"))
(*html*
      <p>$a Tutoeliom.coucoutext sp <:xmllist< Try it >> ()$.</p>

      </div>
    </div>
    <h3 id="p1moreexamples">More examples</h3>
    <div class="onecol">
      <p>Services registered with <code>register_new_service</code>
         are available for all users. We call them <em>public services</em>.
      </p>
      <p>
        Page generation may have side-effects:
      </p>
*html*)
let count =
  let next =
    let c = ref 0 in
      (fun () -> c := !c + 1; !c)
  in
  register_new_service
    ~path:["count"]
    ~get_params:unit
    (fun _ () () ->
      return
        (html
         (head (title (pcdata "counter")) [])
         (body [p [pcdata (string_of_int (next ()))]])))
(*html*
      <p>
      See this example $a Tutoeliom.count sp <:xmllist< here >> ()$.
      </p>
      <p>As usual in OCaml, you can forget labels when the application
          is total:</p>
*html*)
let hello =
  register_new_service
    ["dir";"hello"]  (* the url dir/hello *)
    unit
    (fun _ () () ->
      return
        (html
         (head (title (pcdata "Hello")) [])
         (body [h1 [pcdata "Hello"]])))
(*html*
      <p>
      See this example $a Tutoeliom.hello sp <:xmllist< here >> ()$.
      </p>



      <p>The last example shows how to define the default page for
       a directory. (Note that <code>["rep";""]</code> means
       the default page of the directory <code>rep/</code>)</p>
*html*)
let default = register_new_service ["rep";""] unit
  (fun _ () () ->
    return
     (html
      (head (title (pcdata "")) [])
      (body [p [pcdata "default page. rep is redirected to rep/"]])))
(*html*
      <p>
      See $a Tutoeliom.default sp <:xmllist< default >> ()$.
      </p>
      <div class="encadre">
        <h4>Remarks on paths</h4>
        <p>
          <code>["foo";"bar"]</code> corresponds to the URL
          <code>foo/bar</code>.<br/>
          <code>["dir";""]</code> corresponds to the URL <code>dir/</code>
          (that is: the default page of the directory <code>dir</code>). <br/>
          The empty list <code>[]</code> is equivalent to <code>[""]</code>.
        </p>
        <p>
          <em>Warning:</em>
          You cannot create a service on path <code>["foo"]</code>
          (URL <code>foo</code>, without slash at the end)
          and another on path <code>["foo";"bar"]</code>
          (URL <code>foo/bar</code>) because <code>foo</code> can not be
          both a directory and a file.
          Be also careful not to use a string as a directory with
          Eliom, if it is a file for Staticmod (and vice versa).
        </p>
        <p>
          <em>Warning:</em>
          <code>["foo";"bar"]</code> is not equivalent to
          <code>["foo/bar"]</code>.
          In the latter, the "/" will be encoded in the URL.
        </p>
      </div>
    </div>
    <h3 id="p1parameters">Parameters</h3>
    <div class="onecol">
      <h4>Typed parameters</h4>
      <p>The parameter labeled
        <code><span class="Clabel">~get_params</span></code>
        indicates the type of GET parameters for the page (that is, parameters
        present in the URL).
        $a ~fragment:"VALunit" ~service:senddoc ~sp [code [pcdata "unit" ]] [version;"Eliom_parameters.html"]$ means that the page does not take any GET parameter.
      </p>
      <p>Functions implementing services are called <em>service handlers</em>.
       They take three parameters. The first
       one has type
       $a ~fragment:"TYPEserver_params" ~service:senddoc ~sp [code [pcdata "Eliom_sessions.server_params" ]]
   [version;"Eliom_sessions.html"]$
        and
       corresponds to server parameters (user-agent, ip, current-url, etc.
       - see later in that section for examples of use),
        the second one is for GET parameters
        (that is, parameters in the URL) and the third one
       for POST parameters (parameters in the body of the HTTP request).</p>
      <p>Here is an example of a service with GET parameters:</p>
*html*)
let writeparams _ (i1, (i2, s1)) () =
  return
   (html
    (head (title (pcdata "")) [])
    (body [p [pcdata "You sent: ";
              strong [pcdata (string_of_int i1)];
              pcdata ", ";
              strong [pcdata (string_of_int i2)];
              pcdata " and ";
              strong [pcdata s1]]]))
(*zap* you can register twice the same service, with different parameter names
 *zap*)
let coucou_params = register_new_service
    ~path:["coucou"]
    ~get_params:(int "i" ** (int "ii" ** string "s"))
    writeparams
(*zap* If you register twice exactly the same URL, the server won't start
 *zap*)
(*html*
      <p>Note that the URLs of <code>coucou</code>
      and <code>coucou_params</code>
      differ only by parameters. Url
      $a Tutoeliom.coucou sp <:xmllist< <code>http://<em>your_server</em>/examples/coucou</code> >> ()$
      will run the first one,<br/>
      $a Tutoeliom.coucou_params sp <:xmllist< <code>http://<em>your_server</em>/examples/coucou?i=42&amp;ii=17&amp;s=krokodile</code> >> (42, (17, "krokodile")) $
      will run the second one.<br/>
      If <code>i</code> is not an integer,
      the server will display an error-message
      (try to change the value in the URL).<br/>
      Here, <code>int</code>, <code>string</code> and <code>**</code>
      are functions defined in the $a ~service:senddoc ~sp [code [pcdata "Eliom_parameters" ]] [version;"Eliom_parameters.html"]$ module.
      <br/>
      <em>Warning:</em>
      The infix function <code>( ** )</code> is to be used to
      construct <em>pairs</em> (not tuples).
      </p>
      <p>The following examples shows how to create a service with "suffix"
         service
         (taking the end of the URL as a parameter, as wikis do very often)
        and how to get server information:</p>
*html*)
let uasuffix =
  register_new_service
    ~path:["uasuffix"]
    ~get_params:(suffix (int "year" ** int "month"))
    (fun sp (year, month) () ->
      return
       (html
        (head (title (pcdata "")) [])
        (body
           [p [pcdata "The suffix of the url is ";
               strong [pcdata ((string_of_int year)^"/"
                               ^(string_of_int month))];
               pcdata ", your user-agent is ";
               strong [pcdata (Eliom_sessions.get_user_agent sp)];
               pcdata ", your IP is ";
               strong [pcdata (Eliom_sessions.get_remote_ip sp)]]])))
(*html*
    <p>This service will answer to URLs like
    <code>http://.../uasuffix/2000/11</code>.</p>
    <p>See $a Tutoeliom.uasuffix sp <:xmllist< <code>uasuffix</code> >> (2007,07)$</p>
    <p>Suffix parameters have names, because we can create forms towards
       these services. <code>uasuffix/2000/11</code> is equivalent to
       <code>uasuffix/?year=2000&amp;month=11</code>.
    </p>
    <p>
       <code>suffix_prod</code> allows to take both a suffix and
       other parameters.<br/>
       <code>all_suffix</code> allows to take the end of the suffix as a
       <code>string list</code>.
    </p>
*html*)
let isuffix =
  register_new_service
    ~path:["isuffix"]
    ~get_params:(suffix_prod (int "suff" ** all_suffix "endsuff") (int "i"))
    (fun sp ((suff, endsuff), i) () ->
      return
       (html
        (head (title (pcdata "")) [])
        (body
           [p [pcdata "The suffix of the url is ";
               strong [pcdata (string_of_int suff)];
               pcdata " followed by ";
               strong [pcdata (Ocsigen_extensions.string_of_url_path endsuff)];
               pcdata " and i is equal to ";
               strong [pcdata (string_of_int i)]]])))
(*html*
      <p>See $a Tutoeliom.isuffix sp <:xmllist< <code>isuffix</code> >> ((11, ["a";"b";"c"]) , 22)$.</p>




      <p>The following example shows how to use your own types :</p>
*html*)
type mysum = A | B
let mysum_of_string = function
  | "A" -> A
  | "B" -> B
  | _ -> raise (Failure "mysum_of_string")
let string_of_mysum = function
  | A -> "A"
  | B -> "B"

let mytype =
  Eliom_predefmod.Xhtml.register_new_service
    ~path:["mytype"]
    ~get_params:
      (Eliom_parameters.user_type mysum_of_string string_of_mysum "valeur")
    (fun _ x () ->
      let v = string_of_mysum x in
      return
        (html
         (head (title (pcdata "")) [])
         (body [p [pcdata (v^" is valid. Now try with another value.")]])))
(*html*
      <p>See $a Tutoeliom.mytype sp <:xmllist< <code>mytype</code> >> Tutoeliom.A$.</p>

      <h4 id="p1any">Untyped parameters</h4>
      <p>If you want a service that answers to requests with any parameters,
      use the $a ~fragment:"VALany" ~service:senddoc ~sp [code [pcdata "Eliom_parameters.any" ]] [version;"Eliom_parameters.html"]$ value. The service will get an
      association list of strings. Example:
      </p>
*html*)
let raw_serv = register_new_service
    ~path:["any"]
    ~get_params:Eliom_parameters.any
  (fun _ l () ->
    let ll =
      List.map
        (fun (a,s) -> << <strong>($str:a$, $str:s$)</strong> >>) l
    in
    return
     << <html>
          <head><title></title></head>
          <body>
          <p>
            You sent:
            $list:ll$
          </p>
          </body>
        </html> >>)
(*html*
      <p>Try $a Tutoeliom.raw_serv sp <:xmllist< <code>raw_serv</code> >>
         [("sun","yellow");("sea","blue")]$.</p>
      <div class="encadre">
        <h4>Catching errors</h4>
        <p>You can catch parameters typing errors and write your own
        error messages using the optional parameter
        <code>error_handler</code>. Example:</p>
*html*)

let catch = register_new_service
    ~path:["catch"]
    ~get_params:(int "i")
    ~error_handler:(fun sp l ->
      return
        (html
         (head (title (pcdata "")) [])
         (body [p [pcdata ("i is not an integer.")]])))
    (fun _ i () ->
      let v = string_of_int i in
      return
        (html
           (head (title (pcdata "")) [])
           (body [p [pcdata ("i is an integer: "^v)]])))
(*html*
      <p><code>error_handler</code> takes as parameters the usual
         <code>sp</code>, and a list of pairs <code>(n,ex)</code>,
         where <code>n</code> is the name of the wrong parameter, and
         <code>ex</code> is the exception that has been raised while
         parsing its value.</p>
      <p>See $a Tutoeliom.catch sp <:xmllist< <code>catch</code> >> 22$ (change the value
   of the parameter).</p>
     </div>
    </div>
    <h3 id="p1links">Links</h3>
    <div class="onecol">
      <p>To create a link (<code>&lt;a&gt;</code>), use the
          $a ~fragment:"VALa" ~service:senddoc ~sp [code [pcdata "Eliom_predefmod.Xhtml.a" ]] [version;"Eliom_predefmod.XHTMLFORMSSIG.html"]$ function (or <code>Eliom_duce.Xhtml.a</code>, etc),
          as in these examples:
      </p>
*html*)
let links = register_new_service ["rep";"links"] unit
 (fun sp () () ->
   return
    (html
     (head (title (pcdata "Links")) [])
     (body
       [p
        [Eliom_predefmod.Xhtml.a coucou sp [pcdata "coucou"] (); br ();
         Eliom_predefmod.Xhtml.a hello sp [pcdata "hello"] (); br ();
         Eliom_predefmod.Xhtml.a default sp
           [pcdata "default page of the dir"] (); br ();
         Eliom_predefmod.Xhtml.a uasuffix sp
           [pcdata "uasuffix"] (2007,06); br ();
         Eliom_predefmod.Xhtml.a coucou_params sp
           [pcdata "coucou_params"] (42,(22,"ciao")); br ();
         Eliom_predefmod.Xhtml.a raw_serv sp
           [pcdata "raw_serv"] [("sun","yellow");("sea","blue and pink")]; br ();
         Eliom_predefmod.Xhtml.a
           (new_external_service
              ~prefix:"http://fr.wikipedia.org"
              ~path:["wiki";""]
              ~get_params:(suffix (all_suffix "suff"))
              ~post_params:unit ())
           sp
           [pcdata "OCaml on wikipedia"]
           ["OCaml"]; br ();
         XHTML.M.a
           ~a:[a_href (uri_of_string "http://en.wikipedia.org/wiki/OCaml")]
           [pcdata "OCaml on wikipedia"]
       ]])))
(*zap*
   Note that to create a link we need to know the current url, because:
   the link from toto/titi to toto/tata is "tata" and not "toto/tata"
*zap*)
(*html*
      <p>See $a Tutoeliom.links sp <:xmllist< <code>links</code> >> ()$.</p>



      <p>If you open $a ~service:senddoc ~sp [code [pcdata "Eliom_predefmod.Xhtml" ]] [version;"Eliom_predefmod.Xhtml.html"]$ after $a ~service:senddoc ~sp [code [pcdata "XHTML.M" ]] [version;"XHTML.M.html"]$,
        $a ~fragment:"VALa" ~service:senddoc ~sp [code [pcdata "Eliom_predefmod.Xhtml.a" ]] [version;"Eliom_predefmod.XHTMLFORMSSIG.html"]$
   will mask $a ~fragment:"VALa" ~service:senddoc ~sp [code [pcdata "XHTML.M.a" ]] [version;"XHTML.M.html"]$.
        Thus you can avoid to write fully qualified values most of the time.
      </p>
      <p>
        $a ~fragment:"VALa" ~service:senddoc ~sp [code [pcdata "Eliom_predefmod.Xhtml.a" ]] [version;"Eliom_predefmod.XHTMLFORMSSIG.html"]$ takes as first parameter
        the service you want to link to.
        Note that to create a (relative) link we need to know the current URL.
        That's why the function <code>a</code>
        takes <code>sp</code> as second parameter.
      </p>
      <p>
      The third parameter is the text of the link.
      The last parameter is for
      GET parameters you want to put in the link.
      The type of this parameter and the name of GET parameters depend
      on the service you link to.
      </p>
      <p>
      The links to Wikipedia shows how to define an external service (here it
      uses a suffix URL).
      For an external service without parameters, you can use the low level
      function $a ~fragment:"VALa" ~service:senddoc ~sp [code [pcdata "XHTML.M.a" ]] [version;"XHTML.M.html"]$, if you don't want to create an
      external service explicitely.
      Note that the path must be a list of strings.
      Do not write <code>["foo/bar"]</code>,
      but <code>["foo";"bar"]</code>, otherwise, the "/" will be encoded in
      the URL.
      </p>
      <p>
        If you want to create (mutually or not) recursive pages,
        create the service using $a ~fragment:"VALnew_service" ~service:senddoc ~sp [code [pcdata "Eliom_services.new_service" ]] [version;"Eliom_services.html"]$ first,
        then register it in the table using (for example)
        $a ~fragment:"VALregister" ~service:senddoc ~sp [code [pcdata "Eliom_predefmod.Xhtml.register" ]] [version;"Eliom_mkreg.ELIOMREGSIG1.html"]$:
      </p>
*html*)
let linkrec = Eliom_services.new_service ["linkrec"] unit ()

let _ = Eliom_predefmod.Xhtml.register linkrec
    (fun sp () () ->
      return
       (html
        (head (title (pcdata "")) [])
        (body [p [a linkrec sp [pcdata "click"] ()]])))
(*zap* If some url are not registered, the server will not start:
let essai =
  new_url
   ~path:["essai"]
   ~server_params:no_server_param
   ~get_params:no_get_param
   ()
*zap*)
(*zap* pour les reload : le serveur ne s'éteint pas mais ajoute un message sur les services non enregistrés dans son log *zap*)
(*html*
      <p>$a Tutoeliom.linkrec sp <:xmllist< See <code>linkrec</code> >> ()$.</p>
      <p> The server will fail on startup if there are any unregistered

      services.</p>
    </div>
    <h3 id="p1forms">Forms</h3>
    <div class="onecol">
      <h4>Forms towards services</h4>
      <p>The function $a ~fragment:"VALget_form" ~service:senddoc ~sp [code [pcdata "Eliom_predefmod.Xhtml.get_form" ]] [version;"Eliom_predefmod.XHTMLFORMSSIG.html"]$ allows to create a form
      that uses the GET method (parameters in the URL).
      It works like $a ~fragment:"VALa" ~service:senddoc ~sp [code [pcdata "Eliom_predefmod.Xhtml.a" ]] [version;"Eliom_predefmod.XHTMLFORMSSIG.html"]$ but takes a <em>function</em> that creates the form from the parameters names as parameter.
      </p>
*html*)
let create_form =
  (fun (number_name, (number2_name, string_name)) ->
    [p [pcdata "Write an int: ";
        Eliom_predefmod.Xhtml.int_input ~input_type:`Text ~name:number_name ();
        pcdata "Write another int: ";
        Eliom_predefmod.Xhtml.int_input ~input_type:`Text ~name:number2_name ();
        pcdata "Write a string: ";
        Eliom_predefmod.Xhtml.string_input ~input_type:`Text ~name:string_name ();
        Eliom_predefmod.Xhtml.string_input ~input_type:`Submit ~value:"Click" ()]])

let form = register_new_service ["form"] unit
  (fun sp () () ->
     let f = Eliom_predefmod.Xhtml.get_form coucou_params sp create_form in
     return
       (html
         (head (title (pcdata "")) [])
         (body [f])))
(*html*
      <p>$a Tutoeliom.form sp <:xmllist< See the function <code>form</code> in action >> ()$.</p>

      <p>Note that if you want to use typed parameters,
       you cannot use functions like $a ~fragment:"VALinput" ~service:senddoc ~sp [code [pcdata "XHTML.M.input" ]] [version;"XHTML.M.html"]$ to
       create your forms (if you want to use parameters defined with
       $a ~fragment:"VALany" ~service:senddoc ~sp [code [pcdata "Eliom_parameters.any" ]] [version;"Eliom_parameters.html"]$, <a href="#p1any">see later</a>). Indeed, parameter names are typed to force them
       be used properly. In our example, <code>number_name</code> has type
       <code>int param_name</code> and must be used with
       <code>int_input</code> (or other widgets), whereas
       <code>string_name</code> has type
       <code>string param_name</code> and must be used with
       <code>string_input</code> (or other widgets).
       All functions for creating form widgets are detailed
       $a ~service:senddoc ~sp [pcdata "here"]
         [version;"Eliom_predefmod.XHTMLFORMSSIG.html"]$.
      </p>

      <p>For untyped forms, you may use functions from XHTML.M (or
      OCamlDuce's syntax, or whatever syntax you are using) or
      functions which name is prefixed by "<code>raw_</code>".
      Here is a form linking to our (untyped) service
      <code>raw_serv</code>.</p>
*html*)
let raw_form = register_new_service
    ~path:["anyform"]
    ~get_params:unit
    (fun sp () () ->
      return
        (html
           (head (title (pcdata "")) [])
           (body
              [h1 [pcdata "Any Form"];
               Eliom_predefmod.Xhtml.get_form raw_serv sp
                 (fun () ->
                   [p [pcdata "Form to raw_serv: ";
                       Eliom_predefmod.Xhtml.raw_input ~input_type:`Text ~name:"plop" ();
                       Eliom_predefmod.Xhtml.raw_input ~input_type:`Text ~name:"plip" ();
                       Eliom_predefmod.Xhtml.raw_input ~input_type:`Text ~name:"plap" ();
                       Eliom_predefmod.Xhtml.string_input ~input_type:`Submit ~value:"Click" ()]])
                ])))
(*html*
      <p>Try this $a Tutoeliom.raw_form sp <:xmllist< form >> ()$.</p>
      <h4>POST parameters</h4>
      <p>
   By default Web page parameters are transferred in the URL (GET parameters).
   A web page may also expect POST parameters
   (that is, parameters that are not in the URL but in the body of the HTTP
   request).
   Use this if you don't want the user to be able to bookmark
   the URL with parameters, for example if you want to post some
   data that will change the state of the server (payment,
   database changes, etc).
   When designing a Web site, think carefully about the choice between
   GET or POST method for each service!
   </p>
   <p>
   When you register a service with POST parameters, you must first register a service (fallback) without these parameters (for example that will
   answer if the page is reloaded without the hidden parameters, or
   if it is bookmarked).
      </p>
*html*)
let no_post_param_service =
  register_new_service
    ~path:["post"]
    ~get_params:unit
    (fun _ () () ->
      return
        (html
         (head (title (pcdata "")) [])
         (body [h1 [pcdata
                      "Version of the page without POST parameters"]])))

let my_service_with_post_params =
  register_new_post_service
    ~fallback:no_post_param_service
    ~post_params:(string "value")
    (fun _ () value ->
      return
        (html
         (head (title (pcdata "")) [])
         (body [h1 [pcdata value]])))
(*html*


      <p>Services may take both GET and POST parameters:</p>
*html*)
let get_no_post_param_service =
  register_new_service
    ~path:["post2"]
    ~get_params:(int "i")
    (fun _ i () ->
      return
        (html
         (head (title (pcdata "")) [])
         (body [p [pcdata "No POST parameter, i:";
                   em [pcdata (string_of_int i)]]])))

let my_service_with_get_and_post = register_new_post_service
  ~fallback:get_no_post_param_service
  ~post_params:(string "value")
  (fun _ i value ->
    return
      (html
         (head (title (pcdata "")) [])
         (body [p [pcdata "Value: ";
                   em [pcdata value];
                   pcdata ", i: ";
                   em [pcdata (string_of_int i)]]])))
(*html*
      <h4 id="p1postforms">POST forms</h4>
       <p> To create a POST form, use the
           $a ~fragment:"VALpost_form" ~service:senddoc ~sp [code [pcdata "Eliom_predefmod.Xhtml.post_form" ]] [version;"Eliom_predefmod.XHTMLFORMSSIG.html"]$ function.
           It is similar to $a ~fragment:"VALget_form" ~service:senddoc ~sp [code [pcdata "Eliom_predefmod.Xhtml.get_form" ]] [version;"Eliom_predefmod.XHTMLFORMSSIG.html"]$
           with an additional parameter
           for the GET parameters you want to put in the URL (if any).
           Here, <code>form2</code> is a page containing a form
           to the service <code>post</code> (using XHTML.M's functions)
           and <code>form3</code> (defined using the syntax extension)
           contains a form to <code>post2</code>, with a GET parameter.
           <code>form4</code> is a form to an external page.
       </p>
*html*)
let form2 = register_new_service ["form2"] unit
  (fun sp () () ->
     let f =
       (Eliom_predefmod.Xhtml.post_form my_service_with_post_params sp
          (fun chaine ->
            [p [pcdata "Write a string: ";
                string_input ~input_type:`Text ~name:chaine ()]]) ()) in
     return
       (html
         (head (title (pcdata "form")) [])
         (body [f])))

let form3 = register_new_service ["form3"] unit
  (fun sp () () ->
     let f  =
       (Eliom_predefmod.Xhtml.post_form my_service_with_get_and_post sp
          (fun chaine ->
            <:xmllist< <p> Write a string:
                    $string_input ~input_type:`Text ~name:chaine ()$ </p> >>)
          222) in
     return
       << <html>
            <head><title></title></head>
            <body>$f$</body></html> >>)

let form4 = register_new_service ["form4"] unit
  (fun sp () () ->
     let f  =
       (Eliom_predefmod.Xhtml.post_form
          (new_external_service
             ~prefix:"http://www.petizomverts.com"
             ~path:["zebulon"]
             ~get_params:(int "i")
             ~post_params:(string "chaine") ()) sp
          (fun chaine ->
            <:xmllist< <p> Write a string:
                     $string_input ~input_type:`Text ~name:chaine ()$ </p> >>)
          222) in
     return
       (html
        (head (title (pcdata "form")) [])
        (body [f])))
(*html*
      <p>See the urls:
      $a Tutoeliom.no_post_param_service sp <:xmllist< <code>post</code> without parameter >> ()$,
      $a Tutoeliom.get_no_post_param_service sp <:xmllist< <code>post2</code> without POST parameter >> 123$,
      $a Tutoeliom.form2 sp <:xmllist< <code>form2</code> >> ()$,
      $a Tutoeliom.form3 sp <:xmllist< <code>form3</code> >> ()$,
      $a Tutoeliom.form4 sp <:xmllist< <code>form4</code> >> ()$.
      </p>

    </div>
    <h3 id="p1threads">Threads</h3>
    <div class="onecol">
      <p>
      Remember that a Web site written with Eliom is an OCaml application.
      This application must be able to handle several requests at the same
      time, in order to prevent a single request from slowing down the whole server. To make this possible, Ocsigen
      is using <em>cooperative threads</em>
      (implemented in monadic style
      by Jérôme Vouillon) which make them really easy
      to use (see $a ~service:senddoc ~sp [code [pcdata "Lwt" ]] [version;"Lwt.html"]$ module).
      </p>
      <p>Take time to read the
        $a lwt sp [pcdata "documentation about "; code [pcdata "Lwt"]] ()$
        right now if you want to understand the following of this tutorial.
      </p>
      <p>As it doesn't cooperate, the following page will stop the
      server for 5 seconds. No one will be able to query the server during
      this period:</p>
<pre><span style="color:green">let</span> looong =
  register_new_service
    <span style="color:#770000">~path:</span>[<span style="color:#aa4444">"looong"</span>]
    <span style="color:#770000">~get_params:</span>unit
    (<span style="color:green">fun</span> sp () () -&gt;
      <span style="color:#0033cc">Unix</span>.sleep 5;
      return
        (html
          (head (title (pcdata <span style="color:#aa4444">""</span>)) [])
          (body [h1 [pcdata <span style="color:#aa4444">"Ok now, you can read the page."</span>]])))</pre>
      <p>To solve this problem, use a cooperative version of
         $a ~fragment:"VALsleep" ~service:senddoc ~sp [code [pcdata "sleep" ]] [version;"Lwt_unix.html"]$:</p>
*html*)
let looong =
  register_new_service
    ~path:["looong"]
    ~get_params:unit
    (fun sp () () ->
      Lwt_unix.sleep 5.0 >>= fun () ->
      return
        (html
          (head (title (pcdata "")) [])
          (body [h1 [pcdata
                   "Ok now, you can read the page."]])))
(*html*



      <p>If you want to use, say, a database library that is not written
       in cooperative way, but is thread safe for preemptive threads,
       use the <code>Lwt_preemptive</code> module to
       detach the computation. In the following example,
       we simulate the request by a call to <code>Unix.sleep</code>:
      </p>
*html*)
let looong2 =
  register_new_service
    ~path:["looong2"]
    ~get_params:unit
    (fun sp () () ->
      Lwt_preemptive.detach Unix.sleep 5 >>= fun () ->
      return
        (html
          (head (title (pcdata "")) [])
          (body [h1 [pcdata
                   "Ok now, you can read the page."]])))
(*html*
      <p>$a Tutoeliom.looong2 sp <:xmllist< See <code>looong2</code> >> ()$.</p>
    </div>

    <h3 id="p1thebigpicture">The big picture</h3>
    <div class="encadre sanstitre">
      <p>
         You now have the minimum knowledge to write basic Web sites with
         Eliom: page typing, service creation, parameters, forms
         and database acces using $a ~service:senddoc ~sp [code [pcdata "Lwt" ]] [version;"Lwt.html"]$
         (and possibly $a ~fragment:"VALdetach" ~service:senddoc ~sp [code [pcdata "Lwt_preemptive.detach" ]] [version;"Lwt_preemptive.html"]$).
         Here is a summary of all other concepts introduced by Eliom.
         They will allow you to easily program more complex behaviours and will be developped in the following sections of this tutorial.
      </p>

      <h4>Different kinds of services</h4>
      <p>
      Before beginning the implementation, think about the URLs you want to
      create as entry points to your Web site, and the services
      you want to provide.
      </p>
      <p>Services we used, so far, are called <em>main services</em>.
      But there are other kinds of services depending on the precise
      behaviour you want for links and forms. Clicking on a link or a form
      may trigger:</p>
      <ul>
        <li>the request of a new document (page) (or not),</li>
        <li>the sending of data to the server using the POST method (or not),</li>
        <li>an action on the server (or not),</li>
        <li>a change of URL (or not).</li>
      </ul>
      <p>To take into account all possible behaviours with respect to URLs, 
        Eliom uses three kinds of services:</p>
      <dl class="blue">
        <dt>Main services</dt><dd>are the main entry points of your sites.
        Created by <code>new_service</code> or
        <code>new_post_service</code>.
        They correspond to the public URLs of your Web site, and will last
        forever.
        </dd>
        <dt>(Attached) coservices</dt><dd>are services that share their
        location (URL) with a main service (fallback).
        They are distinguished from that main service using a special parameter
        (added automatically by Eliom), containing either the name of the
        coservice or a number generated automatically.
        They are often created dynamically for one user
        (usually in the session table), depending on previous interaction
        during the session.
        In general, they disappear after a timeout letting the fallback answer
        afterwards.
        Another use of (POST) coservices is to customize one
        button but not the page it leads to (like the disconnect button
        in the example of sessions with <em>actions</em> as below).
        </dd>
        <dt>Non-attached coservices</dt><dd>are
        coservices that are not
        attached to a particular URL. A link towards a non-attached
        coservice will go to the current URL with a special parameter
        containing either the name of the service, or a number generated 
        automatically (each time different).
        It is useful when you want the same link or form on several pages
        (for example a connection box) but you don't want to go to another
        URL. Non-attached coservices are often used with <em>actions</em>
        (see below).
        </dd>
      </dl>
      <p>To summarize, if you want to go to another URL, use
      (attached) (co)services. If you want to stay on the same URL
      use non-attached coservices.</p>
      <h4>GET or POST?</h4>
      <p>Each of these services both have a version with GET parameters and
      another with POST parameters.</p>
      <p>
      POST and GET parameters are not equivalent, and you must be very careful
      if you want to use one or the other.<br/>
      GET parameters are the parameters you see in the URL (for
      example <code>http://<em>your_server</em>/examples/coucou?i=42&amp;ii=17&amp;s=krokodile</code>). They are created by browsers if you use forms with the GET method, or written directly in the URL.<br/>
      POST parameters are sent by browsers in the body of the HTTP request. That's the only solution
      if you want to send files with your request.
      </p>
      <p>
      Remember that only pages without POST parameters are bookmarkable.
      Use GET parameters if you want the user to be able to come back to the URL
      later or to write the URL manually.<br/>
      Use POST parameters when you want a different behaviour
      between the first click and a reload of the page. Usually sending
      POST parameters triggers an action on server side
      (like a payment, or adding something in a database), and you don't want
      it to succeed several times if the page is reloaded or bookmarked.</p>

    </div>
    <div class="encadre sanstitre">
      <h4>Data returned by services</h4>
      <p>Services can send several types of data,
      using these different modules:</p>
        <table>
<tr><td class="empty"></td>
  <th class="col2">Services</th>
  <th colspan="2" class="col2">Coservices</th></tr>
<tr><td class="empty"></td>
  <th class="col2"></th>
  <th class="col2">attached<br/>named&nbsp;/&nbsp;anonymous</th>
  <th class="col2">non-attached<br/>named&nbsp;/&nbsp;anonymous</th>
</tr>
<tr><th class="row">$a ~service:senddoc ~sp [code [pcdata "Eliom_predefmod.Xhtml" ]] [version;"Eliom_predefmod.Xhtml.html"]$</th>
          <td colspan="4">Allows to register functions that
        generate xhtml pages
        statically checked using polymorphic variant types. You may use
        constructor functions from $a ~service:senddoc ~sp [code [pcdata "XHTML.M" ]] [version;"XHTML.M.html"]$ or a syntax
        extension close to the standard xhtml syntax.
          </td></tr>
<tr><th class="row">$a ~service:senddoc ~sp [code [pcdata "Eliom_predefmod.Xhtmlcompact" ]] [version;"Eliom_predefmod.Xhtmlcompact.html"]$</th>
          <td colspan="4">Same, but without pretty printing (does not add
            spaces or line breaks).
          </td></tr>
<tr><th class="row">$a ~service:senddoc ~sp [code [pcdata "Eliom_predefmod.Blocks" ]] [version;"Eliom_predefmod.Blocks.html"]$</th>
          <td colspan="4">Allows to register functions that
        generate a portion of page (content of the body tag) using
        $a ~service:senddoc ~sp [code [pcdata "XHTML.M" ]] [version;"XHTML.M.html"]$ or the syntax extension.
        (useful for <code>XMLHttpRequest</code> requests for example).

          </td></tr>
<tr><th class="row">$a ~service:senddoc ~sp [code [pcdata "Eliom_duce.Xhtml" ]] [version;"Eliom_duce.Xhtml.html"]$</th>
          <td colspan="4">Allows to register functions
            that generate xhtml pages
        statically checked using <code>OCamlduce</code>. Typing is
        stricter, and you need a modified version of the OCaml compiler
        (OCamlduce).
          </td></tr>
<tr><th class="row">$a ~service:senddoc ~sp [code [pcdata "Eliom_predefmod.HtmlText" ]] [version;"Eliom_predefmod.HtmlText.html"]$</th>
          <td colspan="4">Allows to register functions that
        generate text html pages, without any typechecking of the content.
        The content type sent by the server is "text/html".

          </td></tr>
<tr><th class="row">$a ~service:senddoc ~sp [code [pcdata "Eliom_predefmod.CssText" ]] [version;"Eliom_predefmod.CssText.html"]$</th>
          <td colspan="4">Allows to register functions that
        generate CSS pages, without any typechecking of the content.
        The content type sent by the server is "text/css".

          </td></tr>
<tr><th class="row">$a ~service:senddoc ~sp [code [pcdata "Eliom_predefmod.Text" ]] [version;"Eliom_predefmod.Text.html"]$</th>
          <td colspan="4">Allows to register functions that
        generate text pages, without any typechecking of the content.
        The services return a pair of strings. The first one is the content
        of the page, the second one is the content type.

          </td></tr>
<tr><th class="row">$a ~service:senddoc ~sp [code [pcdata "Eliom_predefmod.Actions" ]] [version;"Eliom_predefmod.Actions.html"]$</th>
          <td colspan="4">Allows to register actions (
        functions that do not generate any page). The URL is reloaded after
        the action.

          </td></tr>
<tr><th class="row">$a ~service:senddoc ~sp [code [pcdata "Eliom_predefmod.Unit" ]] [version;"Eliom_predefmod.Unit.html"]$</th>
          <td colspan="4">is like $a ~service:senddoc ~sp [code [pcdata "Eliom_predefmod.Actions" ]] [version;"Eliom_predefmod.Actions.html"]$ but the
        URL is not reloaded after the action.
          </td></tr>
<tr><th class="row">$a ~service:senddoc ~sp [code [pcdata "Eliom_predefmod.Redirection" ]] [version;"Eliom_predefmod.Redirection.html"]$</th>
          <td colspan="4"><strong>[New in 1.1.0]</strong> Allows to register HTTP permanent redirections.
            You register the URL of the page you want to redirect to.
            Warning: According to the RFC of the HTTP protocol,
            the URL must be absolute!<br/>
            The browser will get a 301 or 307 code in answer and
            redo the request to the new URL.
            To specify whether you want temporary (307) or
            permanent (301) redirections,
            use the <code>?options</code> parameter of registration functions.
            For example:
            <code>register ~options:`Permanent ...</code> or
            <code>register ~options:`Temporary ...</code>.
          </td></tr>
<tr><th class="row">$a ~service:senddoc ~sp [code [pcdata "Eliom_predefmod.Files" ]] [version;"Eliom_predefmod.Files.html"]$</th>
          <td colspan="4">Allows to register services that send files
          </td></tr>
<tr><th class="row">$a ~service:senddoc ~sp [code [pcdata "Eliom_predefmod.Any" ]] [version;"Eliom_predefmod.Any.html"]$</th>
          <td colspan="4">Allows to register services that can choose
            what they send, for example an xhtml page
            or a file, depending on some situation (parameter, user logged or
            not, page present in a cache ...).
          </td></tr>
        </table>
      <p>It is also possible to create your own modules for other types
      of pages.</p>
    </div>
    <div class="encadre sanstitre">
      <h4>Public and session service tables</h4>
      <p>Each of these registrations may be done in the <em>public</em>
      service table, or in a <em>session</em> service table,
      accessible only to a single user of the Web site. This allows to generate
      custom services for a specific user.
      </p>

      <p>Eliom will try to find the page, in that order:</p>
      <ul>
       <li>in the session service table,</li>
       <li>in the public service table,</li>
       <li>the fallback in the session table, if the coservice has expired,</li>
       <li>the fallback in the public table, if the session has expired.</li>
      </ul>

      <h4>Session data tables</h4>
      <p>It is also possible to create a session data table, where you can
      save information about the session. Each service can look in that table
      to see if a session is opened or not and get the data.
      </p>

    </div>


    <div class="encadre sanstitre">
      <h4>Examples </h4>
      <p>The most commonly used services are:</p>
      <ul>
        <li>Main services (GET or POST) in public service table for public
          pages,
        </li>
        <li>GET attached coservices in session service table to make the
          browser's "back" button turn back in the past, and to allow several
          tabs on different versions of the same page,
        </li>
        <li>Actions registered on POST named non-attached coservices
          to make an effect
          on the server, from any page, and without changing the URL
          (connection/disconnection for example).
        </li>
      </ul>
      <p>Here is a list of frequent issues and the solution Eliom provides to
        to solve them. Most of them will be developped in the following parts of the tutorial.</p>
      <dl>
        <dt>Display the result of a search (plane ticket,
          search engines&nbsp;...)</dt>
        <dd>Use a coservice (anonymous, with timeout) 
          in the session service table.</dd>
        <dt>Keep information about the session (name of the user&nbsp;...)</dt>
        <dd>Use a session data table.</dd>
        <dt>A connection or disconnection box on each page of your site</dt>
        <dd>Use actions registered on named non-attached coservices to set or
         remove data from a session data table.
        </dd>
        <dt>Add something in a shopping basket</dt>
        <dd>Use an action registered on a non-attached coservice,
          with the names of the items as parameters. The action saves the shopping
          basket in a session data table. Thus, the shopping basket will remain
          even if the user pushes the back button of his browser.
        </dd>
        <dt>Book a ticket (in several steps)</dt>
        <dd>Each step creates new (GET) coservices (with or without
          parameters, all attached to the service displaying the main
          booking page)
          according to the data entered by the user. These
          coservices are registered in the session table (with a timeout for
          the whole session or for each of them). Thus the user can go back
          to a previous state, or keep several proposals on differents
          tabs before choosing one.
        </dd>
        <dt>...</dt>
        <dd><em>Help us to complete this list by giving your examples or
          asking questions about other cases! Thank you!</em></dd>
        <dt></dt>
        <dd></dd>
      </dl>
    </div>

°°


let part2 sp =
°:xmllist°


   <h2>2. Sessions, users and other common situations in Web sites</h2>

    <div class="onecol">
      <p>When programming dynamic Web sites, you often want to personalise
       the behaviour and content for one user. To do this, you need to recognise
       the user and save and restore its data. Eliom implements several
       high level features to do that:
      </p>
      <ul>
        <li>Session data tables,</li>
        <li>Session service tables, where you can save private versions of
          main services or new coservices,
        </li>
        <li>Coservices, that may be created dynamically with respect to
          previous interaction with the user.
        </li>
      </ul>
      <p>Eliom is using cookies to recognize users.
         One cookie is automatically set for each user when needed and
         destroyed when the session is closed.
      </p>
      <p>Coservices, but also <em>actions</em>, are also means to control
        precisely the behaviour of the site and to implement easily very
        common situations that require a lot of programming work with
        other Web programming tools. We'll have a lot at some examples in that
        section.
      </p>
    </div>


    <h3 id="p2sessiondata">Session data</h3>
    <div class="onecol">
      <p>
      Eliom provides a way to save session data on server side and
      restore it at each request. This data is available during the whole
      duration of the session.
      To save session data, create a table using
      $a ~fragment:"VALcreate_volatile_table" ~service:senddoc ~sp [code [pcdata "Eliom_sessions.create_volatile_table" ]] [version;"Eliom_sessions.html"]$
      and save and get data from
      this table using $a ~fragment:"VALset_volatile_session_data" ~service:senddoc ~sp [code [pcdata "Eliom_sessions.set_volatile_session_data" ]] [version;"Eliom_sessions.html"]$ and
      $a ~fragment:"VALget_volatile_session_data" ~service:senddoc ~sp [code [pcdata "Eliom_sessions.get_volatile_session_data" ]] [version;"Eliom_sessions.html"]$. The following example shows
      a site with authentification. The name of the user is asked in the login
      form and saved in a table to be displayed on the page instead of the login
      form while the user is connected. Note that the session is opened
      automatically when needed.
      </p>
*html*)
(************************************************************)
(************ Connection of users, version 1 ****************)
(************************************************************)

(*zap* *)
let session_name = "session_data"
(* *zap*)

(* "my_table" will be the structure used to store
   the session data (namely the login name): *)

let my_table = Eliom_sessions.create_volatile_table ()


(* -------------------------------------------------------- *)
(* Create services, but do not register them yet:           *)

let session_data_example =
  Eliom_services.new_service
    ~path:["sessdata"]
    ~get_params:Eliom_parameters.unit
    ()

let session_data_example_with_post_params =
  Eliom_services.new_post_service
    ~fallback:session_data_example
    ~post_params:(Eliom_parameters.string "login")
    ()

let session_data_example_close =
  Eliom_services.new_service
    ~path:["close"]
    ~get_params:Eliom_parameters.unit
    ()



(* -------------------------------------------------------- *)
(* Handler for the "session_data_example" service:          *)

let session_data_example_handler sp _ _  =
  let sessdat = Eliom_sessions.get_volatile_session_data (*zap* *) ~session_name (* *zap*) ~table:my_table ~sp () in
  return
    (html
       (head (title (pcdata "")) [])
       (body
          [
           match sessdat with
           | Eliom_sessions.Data name ->
               p [pcdata ("Hello "^name);
                  br ();
                  Eliom_predefmod.Xhtml.a
                    session_data_example_close
                    sp [pcdata "close session"] ()]
           | Eliom_sessions.Data_session_expired
           | Eliom_sessions.No_data ->
               Eliom_predefmod.Xhtml.post_form
                 session_data_example_with_post_params
                 sp
                 (fun login ->
                   [p [pcdata "login: ";
                       Eliom_predefmod.Xhtml.string_input
                         ~input_type:`Text ~name:login ()]]) ()
         ]))

(* -------------------------------------------------------- *)
(* Handler for the "session_data_example_with_post_params"  *)
(* service with POST params:                                *)

let session_data_example_with_post_params_handler sp _ login =
  Eliom_sessions.close_session (*zap* *) ~session_name (* *zap*) ~sp () >>= fun () ->
  Eliom_sessions.set_volatile_session_data (*zap* *) ~session_name (* *zap*) ~table:my_table ~sp login;
  return
    (html
       (head (title (pcdata "")) [])
       (body
          [p [pcdata ("Welcome " ^ login ^ ". You are now connected.");
              br ();
              Eliom_predefmod.Xhtml.a session_data_example sp [pcdata "Try again"] ()
            ]]))



(* -------------------------------------------------------- *)
(* Handler for the "session_data_example_close" service:    *)

let session_data_example_close_handler sp () () =
  let sessdat = Eliom_sessions.get_volatile_session_data (*zap* *) ~session_name (* *zap*) ~table:my_table ~sp () in
  Eliom_sessions.close_session (*zap* *) ~session_name (* *zap*) ~sp () >>= fun () ->
  return
    (html
       (head (title (pcdata "Disconnect")) [])
       (body [
        (match sessdat with
        | Eliom_sessions.Data_session_expired -> p [pcdata "Your session has expired."]
        | Eliom_sessions.No_data -> p [pcdata "You were not connected."]
        | Eliom_sessions.Data _ -> p [pcdata "You have been disconnected."]);
        p [Eliom_predefmod.Xhtml.a session_data_example sp [pcdata "Retry"] () ]]))


(* -------------------------------------------------------- *)
(* Registration of main services:                           *)

let () =
  Eliom_predefmod.Xhtml.register
    session_data_example_close session_data_example_close_handler;
  Eliom_predefmod.Xhtml.register
    session_data_example session_data_example_handler;
  Eliom_predefmod.Xhtml.register
    session_data_example_with_post_params
    session_data_example_with_post_params_handler


(*zap* old version:
type session_info = string

let my_table = create_volatile_table ()

let sessdata = new_service ["sessdata"] unit ()

let sessdata_with_post_params = new_post_service sessdata (string "login") ()

let close = register_new_service
    ~path:["disconnect"]
    ~get_params:unit
    (fun sp () () ->
      Eliom_sessions.close_session (*zap* *) ~session_name (* *zap*) ~sp () >>=
      (fun () ->
        return
          (html
             (head (title (pcdata "Disconnect")) [])
             (body [p [pcdata "You have been disconnected. ";
                       a sessdata sp [pcdata "Retry"] () ]]))))

let _ = register
    sessdata
    (fun sp _ _ ->
      let sessdat = Eliom_sessions.get_volatile_session_data table:my_table ~sp () in
      return
        (html
           (head (title (pcdata "")) [])
           (body
              [match sessdat with
              | Eliom_sessions.Data name ->
                  p [pcdata ("Hello "^name); br ();
                     a close sp [pcdata "close session"] ()
                   ]
              | Eliom_sessions.Data_session_expired
              | Eliom_sessions.No_data ->
                  post_form sessdata_with_post_params sp
                    (fun login ->
                      [p [pcdata "login: ";
                          string_input ~input_type:`Text ~name:login ()]]) ()
             ])))
let _ = register
    sessdata_with_post_params
    (fun sp _ login ->
      Eliom_sessions.close_session (*zap* *) ~session_name (* *zap*) ~sp () >>=
      (fun () ->
        Eliom_sessions.set_volatile_session_data my_table sp login;
        return
          (html
             (head (title (pcdata "")) [])
             (body
                [p [pcdata ("Welcome "^login^
                            ". You are now connected."); br ();
                    a sessdata sp [pcdata "Try again"] ()
                  ]]))))
*zap*)
(*html*
      <p>
      $a Tutoeliom.session_data_example sp <:xmllist< See this example here >> ()$.
      </p>
      <p>
       To close a session, use the function
                <span class="Cem">$a ~fragment:"VALclose_session" ~service:senddoc ~sp [code [pcdata "Eliom_sessions.close_session" ]] [version;"Eliom_sessions.html"]$</span>.
       Session data will disappear when the session is closed (explicitely
       or by timeout).
       Warning: if your session data contains opened file descriptors,
       they won't be closed by OCaml's garbage collector. Close it yourself!
       (for example using <code>Gc.finalise</code>).
      </p>
      <p>We will see in the following of this tutorial how to improve
      this example to solve the following problems:
      </p>
      <ul>
        <li>
          The use of a main service for disconnection is not a good idea
          for usability. You probably want to go to the same page
          with the login form. We will do this with a coservice.
        </li>
        <li>
          If you want the same login form on several pages, it is tedious
          work to create a coservice with POST parameters for each page.
          We will se how to solve this using actions and named non-attached
          coservices.
        </li>
        <li>
          Session data are kept in memory and will be lost if you switch off
          the server, which is bad if you want long duration sessions.
          You can solve this problem by using persistent tables.
        </li>
      </ul>
    </div>
    <h3 id="p2sessionservices">Session services</h3>
    <div class="onecol">
      <p>
      Eliom allows to replace a public service by a service valid only for
      one user.
      Use this to personalise main services for one user (or to create new
      coservices available only to one user, <a href="#p2calc">see later</a>).
      To create a "session service", register the service in
      a "session service table" (valid only for one client)
      instead of the public table. To do that,
      use <span class="Cem"><code>register_for_session</code></span>
      (for example
     <span class="Cem">$a ~fragment:"VALregister_for_session" ~service:senddoc ~sp [code [pcdata "Eliom_predefmod.Xhtml.register_for_session" ]] [version;"Eliom_mkreg.ELIOMREGSIG1.html"]$</span>).<br/>
      </p><p>
      Users are recognized automatically using a cookie.
      Use this for example if you want two versions of each page,
      one public, one for connected users.
      <br/>
      To close a session, use
                <span class="Cem">$a ~fragment:"VALclose_session" ~service:senddoc ~sp [code [pcdata "Eliom_sessions.close_session" ]] [version;"Eliom_sessions.html"]$</span>.
      Both the session service table and the session data table for that user
      will disappear when the session is closed.
      </p>
      <p>Note that <code>register_for_session</code>
         and <code>close_session</code> take <code>sp</code> as parameter
         (because sp contains the session table).</p>
      <p>The following example shows how to reimplement the previous one
      (<code>session_data_example</code>),
      without using $a ~fragment:"VALset_volatile_session_data" ~service:senddoc ~sp [code [pcdata "Eliom_sessions.set_volatile_session_data" ]] [version;"Eliom_sessions.html"]$.
      Note that this version is less efficient than the other if your site
      has lots of pages, because it requires to register all the new services
      each time a user logs in. But in other cases, that feature is really
      useful, for example with coservices (see
      <a href="#p2coservicesinsessiontable">later</a>).
      </p>
      <p>
      We first define the main page, with a login form:
      </p>
*html*)(*zap* *)
let () = set_default_volatile_session_timeout (Some 3600.)
let () = set_default_persistent_data_session_timeout (Some 86400.)
(* *zap*)
(************************************************************)
(************ Connection of users, version 2 ****************)
(************************************************************)

(*zap* *)
let session_name = "session_services"
(* *zap*)
(* -------------------------------------------------------- *)
(* Create services, but do not register them yet:           *)

let session_services_example =
  Eliom_services.new_service
    ~path:["sessionservices"]
    ~get_params:Eliom_parameters.unit
    ()

let session_services_example_with_post_params =
  Eliom_services.new_post_service
    ~fallback:session_services_example
    ~post_params:(Eliom_parameters.string "login")
    ()

let session_services_example_close =
  Eliom_services.new_service
    ~path:["close2"]
    ~get_params:Eliom_parameters.unit
    ()


(* ------------------------------------------------------------- *)
(* Handler for the "session_services_example" service:           *)
(* It displays the main page of our site, with a login form.     *)

let session_services_example_handler sp () () =
  let f =
    Eliom_predefmod.Xhtml.post_form
      session_services_example_with_post_params
      sp
      (fun login ->
        [p [pcdata "login: ";
            string_input ~input_type:`Text ~name:login ()]]) ()
  in
  return
    (html
       (head (title (pcdata "")) [])
       (body [f]))


(* ------------------------------------------------------------- *)
(* Handler for the "session_services_example_close" service:     *)

let session_services_example_close_handler sp () () =
  Eliom_sessions.close_session (*zap* *) ~session_name (* *zap*) ~sp () >>= fun () ->
  return
    (html
       (head (title (pcdata "Disconnect")) [])
       (body [p [pcdata "You have been disconnected. ";
                 a session_services_example
                   sp [pcdata "Retry"] ()
               ]]))

(*html*


    <p>When the page is called with login parameters,
       it runs the function <code>launch_session</code>
       that replaces some services already defined by new ones:
    </p>
*html*)
(* ------------------------------------------------------------- *)
(* Handler for the "session_services_example_with_post_params"   *)
(* service:                                                      *)

let launch_session sp () login =

  (* New handler for the main page: *)
  let new_main_page sp () () =
    return
      (html
       (head (title (pcdata "")) [])
       (body [p [pcdata "Welcome ";
                 pcdata login;
                 pcdata "!"; br ();
                 a coucou sp [pcdata "coucou"] (); br ();
                 a hello sp [pcdata "hello"] (); br ();
                 a links sp [pcdata "links"] (); br ();
                 a session_services_example_close
                   sp [pcdata "close session"] ()]]))
  in

  (* If a session was opened, we close it first! *)
  Eliom_sessions.close_session (*zap* *) ~session_name (* *zap*) ~sp () >>= fun () ->

  (* Now we register new versions of main services in the
     session service table: *)
  Eliom_predefmod.Xhtml.register_for_session (*zap* *) ~session_name (* *zap*)
    ~sp
    ~service:session_services_example
    (* service is any public service already registered,
       here the main page of our site *)
    new_main_page;

  Eliom_predefmod.Xhtml.register_for_session (*zap* *) ~session_name (* *zap*)
    ~sp
    ~service:coucou
    (fun _ () () ->
      return
        (html
         (head (title (pcdata "")) [])
         (body [p [pcdata "Coucou ";
                   pcdata login;
                   pcdata "!"]])));

  Eliom_predefmod.Xhtml.register_for_session (*zap* *) ~session_name (* *zap*)
    ~sp
    ~service:hello
    (fun _ () () ->
      return
        (html
         (head (title (pcdata "")) [])
         (body [p [pcdata "Ciao ";
                   pcdata login;
                   pcdata "!"]])));

  new_main_page sp () ()

(* -------------------------------------------------------- *)
(* Registration of main services:                           *)

let () =
  Eliom_predefmod.Xhtml.register
    ~service:session_services_example
    session_services_example_handler;
  Eliom_predefmod.Xhtml.register
    ~service:session_services_example_close
    session_services_example_close_handler;
  Eliom_predefmod.Xhtml.register
    ~service:session_services_example_with_post_params
    launch_session
(*zap* Registering for session during initialisation is forbidden:
let _ = register_for_session
    ~path:coucou1
    << <html>
         <head><title></title></head>
         <body><h1>humhum</h1></body>
       </html> >>
*zap*)
(*html*
      <p>
      $a Tutoeliom.session_services_example sp <:xmllist< See the result >> ()$.</p>
      <p>Warning: As in the previous example,
       to implement such connection and disconnection forms, you
       get more flexibility by using <em>actions</em> instead of xhtml services
       (see below for the same example with actions).
      </p>
      <p>Services registered in session tables are called
       <em>session</em> or <em>private</em> services.
       Services registered in the public table
       are called <em>public</em>.
      </p>
    </div>
    <h3 id="p2coservices">Coservices</h3>
    <div class="onecol">
      <p>
   A coservice is a service that uses the same URL as
   a main service, but generates another page.
   They are distinguished from main services only by a special
   parameter, called <em>state</em> parameter.
   Coservices may use GET or POST parameters.</p>
   <p>Most of the time, GET coservices are created dynamically with
   respect to previous interaction with the user and are registered
   in the session table. They allow to give a precise semantics to the
   "back" button of the browser (be sure that you will go back in the
   past) or bookmarks, or duplication of the browser's window.
   (See the <a href="#p2calc"><code>calc</code></a> example below).
   </p>
   <p>
   Use POST coservices if you want to particularize a link or form,
   but not the URL it points to.
   More precisely, POST coservices are mainly used in two situations:
    </p>
   <ul>
   <li>For the same purpose as GET coservices (new services
   corresponding to precise points of the interaction with the user)
   but when you don't want this service to be bookmarkable.</li>
   <li>To create a button that leads to a service after having performed
   a side-effect. For example a disconnection button that leads to the main
   page of the side, but with the side effect of disconnecting the user.</li>
   </ul>
   <p>
   To create a coservice, use
   <span class="Cem">$a ~fragment:"VALnew_coservice" ~service:senddoc ~sp [code [pcdata "Eliom_services.new_coservice" ]] [version;"Eliom_services.html"]$</span> and
   <span class="Cem">$a ~fragment:"VALnew_post_coservice" ~service:senddoc ~sp [code [pcdata "Eliom_services.new_post_coservice" ]] [version;"Eliom_services.html"]$</span>.
   Like $a ~fragment:"VALnew_post_service" ~service:senddoc ~sp [code [pcdata "Eliom_services.new_post_service" ]] [version;"Eliom_services.html"]$,
   they take a public service as parameter
   (labeled <code><span class="Clabel">fallback</span></code>)
   to be used as fallback when the user comes back without the state
   parameter (for example if it was a POST coservice and/or the coservice
   has expired).</p>
   <p>The following example shows the difference between GET coservices
   (bookmarkable) and POST coservices:</p>
*html*)
(************************************************************)
(************** Coservices. Basic examples ******************)
(************************************************************)

(* -------------------------------------------------------- *)
(* We create one main service and two coservices:           *)

let coservices_example =
  Eliom_services.new_service
    ~path:["coserv"]
    ~get_params:Eliom_parameters.unit
    ()

let coservices_example_post =
  Eliom_services.new_post_coservice
    ~fallback:coservices_example
    ~post_params:Eliom_parameters.unit
    ()

let coservices_example_get =
  Eliom_services.new_coservice
    ~fallback:coservices_example
    ~get_params:Eliom_parameters.unit
    ()


(* -------------------------------------------------------- *)
(* The three of them display the same page,                 *)
(* but the coservices change the counter.                   *)

let _ =
  let c = ref 0 in
  let page sp () () =
    let l3 = Eliom_predefmod.Xhtml.post_form coservices_example_post sp
        (fun _ -> [p [Eliom_predefmod.Xhtml.string_input
                        ~input_type:`Submit
                        ~value:"incr i (post)" ()]]) ()
    in
    let l4 = Eliom_predefmod.Xhtml.get_form coservices_example_get sp
        (fun _ -> [p [Eliom_predefmod.Xhtml.string_input
                        ~input_type:`Submit
                        ~value:"incr i (get)" ()]])
    in
    return
      (html
       (head (title (pcdata "")) [])
       (body [p [pcdata "i is equal to ";
                 pcdata (string_of_int !c); br ();
                 a coservices_example sp [pcdata "reload"] (); br ();
                 a coservices_example_get sp [pcdata "incr i"] ()];
              l3;
              l4]))
  in
  Eliom_predefmod.Xhtml.register coservices_example page;
  let f sp () () = c := !c + 1; page sp () () in
  Eliom_predefmod.Xhtml.register coservices_example_post f;
  Eliom_predefmod.Xhtml.register coservices_example_get f
(*html*

      <p>Try $a Tutoeliom.coservices_example sp <:xmllist< <code>coserv</code> >> ()$.</p>
      <p>Note that if the coservice does not exist (for example it
      has expired), the fallback is called.</p>
      <p>In this example, coservices do not take any parameters
      (but the state parameter), but you can create coservices with
      parameters. Note that the fallback of a GET coservice cannot take
      parameters. Actually as coservices parameters have special
      names, it is possible to use a "pre-applied" service as fallback
      (<a href="#p3preapplied">see later</a>).</p>

      <p><strong>Exercise:</strong> Rewrite the example of Web site with
        connection (<code>session_data_example</code>, with session data)
        using a POST
        coservice without parameter to make the disconnection link go back
        to the main page of the site instead of a "disconnection" page.
        It is better for ergonomics, but it would be even better to stay
        on the same page&nbsp;... How to do that with POST coservices?
        A much better solution will be seen in the
        <a href="#p2actions">section
        about actions and non-attached coservices</a>.
      </p>
      <div class="encadre">
        <h4>URLs</h4>
          <p>While designing a Web site, think carefully about the URLs you
          want to use. URLs are the entry points of your site. Think that
          they may be bookmarked. If you create a link, you want to go to
          another URL, and you want a page to be generated. That page may be
          the default page for the URL (the one you get when you go back
          to a bookmarked page), or another page, that depends on the precise
          link or form you used to go to that URL (link to a coservice,
          or page depending on post data).
          Sometimes, you want that clicking
          a link or submitting a form does something without changing the URL.
          You can do this using <em>non-attached coservices</em> (see below).
          </p>
      </div>
      <div class="encadre">
        <h4>Continuations</h4>
        <p>Eliom is using the concept of <em>continuation</em>.
        A continuation represents the future of a program (what to do after).
        When a user clicks on a link or a form, he chooses the future of the
        computation. When he uses the "back" button of the browser, he chooses
        to go back to an old continuation. Continuations for Web programming
        have been introduced by
        $a (new_external_service
             "http://www-spi.lip6.fr"
             ["~queinnec";"PDF";"www.pdf"]
             unit unit ()) sp <:xmllist< Christian Queinnec >> ()$,
        and are a big step in
        the understanding of Web interaction.</p>
        <p>
        Some programming languages (Scheme...) allow to manipulate
        continuations using <em>control operators</em> (like
        <code>call/cc</code>). The style of programming used by Eliom
        is closer to <em>Continuation Passing Style</em> (CPS), and has the
        advantage that it does not need control operators, and fits
        very well Web programming.
        </p>
        <p>Coservices allow to create dynamically
        new continuations that depend on previous interactions with users
        (<a href="#p2calc">See the <code>calc</code> example below</a>).
        Such a behaviour is difficult to simulate with traditional Web
        programming.</p>
        <p>If you want continuations dedicated to a particular user
        register them in the session table.</p>
      </div>
      <h4>Non-attached coservices</h4>
       <p>
       Non-attached coservices are coservices
       that are not attached to an URL path.
       When you do a link or a form towards such a service, the URL do not
       change. The name of the service is sent as a special parameter.
       </p>
       <p>As for attached coservices, there are GET and POST versions.</p>
       <p>
       To create them, use
       $a ~fragment:"VALnew_coservice'" ~service:senddoc ~sp [code [pcdata "Eliom_services.new_coservice'" ]] [version;"Eliom_services.html"]$ or
       $a ~fragment:"VALnew_post_coservice'" ~service:senddoc ~sp [code [pcdata "Eliom_services.new_post_coservice'" ]] [version;"Eliom_services.html"]$.
       POST non-attached coservices are really useful if you want a
       link or form to be present on every page but you don't want the
       URL to change. Very often, non-attached POST coservices are
       used with <em>actions</em> or <em>redirections</em>
       (<a href="#p2actions">see more details and an example in the section about
          actions below</a>).
       </p>
       <p>Non-attached coservices are distinguished by there names
         (if the optional <code>name</code> parameter is present), or a number
         (automatically generated and every times different).</p>
    </div>
    <h3 id="p2coservicesinsessiontable">Coservices in session tables</h3>
    <div id="p2calc" class="onecol">
    <p>You can register coservices in session tables to create
       dynamically new services dedicated to an user.
       Here is an example of pages that add two integers.
       Once the first number is sent by the user, a coservice
       is created and registered in the session table. This service
       takes the second number as parameter and displays the result of
       the sum with the first one.
       Try to duplicate the pages and/or to use the back button of your
       navigator to verify that it has the expected behaviour.</p>
*html*)(*zap* ------------------------------------------------------------------ *)
(* You can register coservices in session tables.
   Use this if you want a link or a form which depends precisely on an
   instance of the web page, for example to buy something on an internet shop.
   UPDATE: Actually it is not a good example, because what we want in a shop
   is the same shoping basket for all pages.
   SEE calc example instead.
*)
(* zap* *)
let session_name = "shop_example"
(* *zap *)
let shop_without_post_params =
  new_service
   ~path:["shop"]
   ~get_params:unit
    ()

let shop_with_post_params =
  new_post_service
    ~fallback:shop_without_post_params
    ~post_params:(string "article")
    ()

let write_shop shop url =
  (post_form shop url
     (fun article ->
        let sb = string_input ~input_type:`Text ~name:article () in
          <:xmllist< <p> What do you want to buy? $sb$ </p> >>) ())

let shop_public_main_page sp () () =
  let f = write_shop shop_with_post_params sp in
  return << <html><body>$f$</body></html> >>

let _ =
  register shop_without_post_params shop_public_main_page


let write_shopping_basket shopping_basket =
  let rec aux = function
      [] -> [ << <br/> >> ]
    | a::l -> let fol = aux l in <:xmllist< $str:a$ <br/> $list:fol$ >>
  in
  let ffol = aux shopping_basket in
    <:xmllist< Your shopping basket: <br/> $list:ffol$ >>

let rec page_for_shopping_basket sp shopping_basket =
  let coshop_with_post_params =
    new_post_coservice
      ~fallback:shop_without_post_params
      ~post_params:(string "article")
      ()
  and copay =
    new_post_coservice
      ~fallback:shop_without_post_params
      ~post_params:unit
      ()
  in
    register_for_session (*zap* *) ~session_name (* *zap*)
      ~sp
      ~service:coshop_with_post_params
      (fun sp () article ->
                 page_for_shopping_basket
                   sp (article::shopping_basket));
    register_for_session (*zap* *) ~session_name (* *zap*)
      ~sp
      ~service:copay
      (fun sp () () ->
        return
           << <html><body>
                <p>You are going to pay:
                  $list:write_shopping_basket shopping_basket$ </p>
              </body></html> >>);
       return << <html>
           <body>
             <div>$list:write_shopping_basket shopping_basket$</div>
             $write_shop coshop_with_post_params sp$
             $post_form copay sp
                    (fun _ -> [p [string_input
                                    ~input_type:`Submit ~value:"pay" ()]]) ()$
           </body>
         </html> >>

let _ = register
  ~service:shop_with_post_params
  (fun sp () article -> page_for_shopping_basket sp [article])
(* *zap*)(*zap* Queinnec example: *zap*)
(************************************************************)
(*************** calc: sum of two integers ******************)
(************************************************************)

(*zap* *)
let session_name = "calc_example"
(* *zap*)
(* -------------------------------------------------------- *)
(* We create two main services on the same URL,             *)
(* one with a GET integer parameter:                        *)

let calc =
  new_service
    ~path:["calc"]
    ~get_params:unit
    ()

let calc_i =
  new_service
    ~path:["calc"]
    ~get_params:(int "i")
    ()


(* -------------------------------------------------------- *)
(* The handler for the service without parameter.           *)
(* It displays a form where you can write an integer value: *)

let calc_handler sp () () =
  let create_form intname =
    [p [pcdata "Write a number: ";
        Eliom_predefmod.Xhtml.int_input ~input_type:`Text ~name:intname ();
        br ();
        Eliom_predefmod.Xhtml.string_input ~input_type:`Submit ~value:"Send" ()]]
  in
  let f = Eliom_predefmod.Xhtml.get_form calc_i sp create_form in
  return
    (html
       (head (title (pcdata "")) [])
       (body [f]))


(* -------------------------------------------------------- *)
(* The handler for the service with parameter.              *)
(* It creates dynamically and registers a new coservice     *)
(* with one GET integer parameter.                          *)
(* This new coservice depends on the first value (i)        *)
(* entered by the user.                                     *)

let calc_i_handler sp i () =
  let create_form is =
    (fun entier ->
       [p [pcdata (is^" + ");
           int_input ~input_type:`Text ~name:entier ();
           br ();
           string_input ~input_type:`Submit ~value:"Sum" ()]])
  in
  let is = string_of_int i in
  let calc_result =
    register_new_coservice_for_session
      ~sp
      ~fallback:calc
      ~get_params:(int "j")
      (fun sp j () ->
        let js = string_of_int j in
        let ijs = string_of_int (i+j) in
        return
          (html
             (head (title (pcdata "")) [])
             (body
                [p [pcdata (is^" + "^js^" = "^ijs)]])))
  in
  let f = get_form calc_result sp (create_form is) in
  return
    (html
       (head (title (pcdata "")) [])
       (body [f]))


(* -------------------------------------------------------- *)
(* Registration of main services:                           *)

let () =
  Eliom_predefmod.Xhtml.register calc   calc_handler;
  Eliom_predefmod.Xhtml.register calc_i calc_i_handler
(*html*
      <p>$a Tutoeliom.calc sp <:xmllist< See the result >> ()$.</p>
    </div>





    <h3 id="p2actions">Actions</h3>
    <div class="onecol">
      <p>Actions are services that do not generate any page.
   Use them to perform an effect on the server (connection/disconnection
   of a user, adding something in a shopping basket, delete a message in
   a forum, etc.). The page you link to is redisplayed after the action.
   For ex, when you have the same form (or link) on several pages
   (for ex a connection form),
   instead of making a version with post params of all these pages,
   you can use only one action, registered on a non-attached coservice.
   To register actions, just use the module $a ~service:senddoc ~sp [code [pcdata "Eliom_predefmod.Actions" ]] [version;"Eliom_predefmod.Actions.html"]$
   instead of $a ~service:senddoc ~sp [code [pcdata "Eliom_predefmod.Xhtml" ]] [version;"Eliom_predefmod.Xhtml.html"]$ (or $a ~service:senddoc ~sp [code [pcdata "Eliom_duce.Xhtml" ]] [version;"Eliom_duce.Xhtml.html"]$, etc.).
   For example
     <span class="Cem">$a ~fragment:"VALregister" ~service:senddoc ~sp [code [pcdata "Eliom_predefmod.Actions.register" ]] [version;"Eliom_predefmod.Actions.html"]$</span>,
     <span class="Cem">$a ~fragment:"VALregister_new_service" ~service:senddoc ~sp [code [pcdata "Eliom_predefmod.Actions.register_new_service" ]] [version;"Eliom_predefmod.Actions.html"]$</span>,
     <span class="Cem">$a ~fragment:"VALregister_for_session" ~service:senddoc ~sp [code [pcdata "Eliom_predefmod.Actions.register_for_session" ]] [version;"Eliom_mkreg.ELIOMREGSIG1.html"]$</span>.<br/>
      </p>
      <p>Here is one simple example. Suppose you wrote a function
        <code>remove</code> to remove one piece of data from a database
        (taking an identifier of the data).
        If you want to put a link on your page to call this function
        and redisplay the page, just create an action like this:
      </p>
<pre>
<span class="Clet">let</span> remove_action <span class="Cnonalphakeyword">=</span>
  <span class="Cconstructor">Eliom_services</span><span class="Cnonalphakeyword">.</span>register_new_post_coservice'
    <span class="Clabel">~post_params:</span><span class="Cnonalphakeyword">(</span><span class="Cconstructor">Eliom_parameters</span><span class="Cnonalphakeyword">.</span>int <span class="Cstring">"id"</span><span class="Cnonalphakeyword">)</span>
    <span class="Cnonalphakeyword">(</span><span class="Cfun">fun</span> sp <span class="Cnonalphakeyword">(</span><span class="Cnonalphakeyword">)</span> id <span class="Cnonalphakeyword">-&gt;</span> remove id &gt;&gt;= <span class="Cfun">fun</span> <span class="Cnonalphakeyword">(</span><span class="Cnonalphakeyword">)</span> <span class="Cnonalphakeyword">-&gt;</span> <span class="Cconstructor">Lwt</span><span class="Cnonalphakeyword">.</span>return <span class="Cnonalphakeyword">[</span><span class="Cnonalphakeyword">]</span><span class="Cnonalphakeyword">)</span></pre>
      <p>Then wherever you want to add a button to do that action
         (on data <code>id</code>),
      create a form like:</p>
<pre>
<span class="Cconstructor">Eliom_predefmod</span><span class="Cnonalphakeyword">.</span><span class="Cconstructor">Xhtml</span><span class="Cnonalphakeyword">.</span>post_form remove_action sp
  <span class="Cnonalphakeyword">(</span><span class="Cfun">fun</span> id_name <span class="Cnonalphakeyword">-&gt;</span>
     <span class="Cconstructor">Eliom_predefmod</span><span class="Cnonalphakeyword">.</span><span class="Cconstructor">Xhtml</span><span class="Cnonalphakeyword">.</span>int_input
       <span class="Clabel">~input_type:</span><span class="Cconstructor">`Hidden</span> <span class="Clabel">~name:</span>id_name <span class="Clabel">~value:</span>id <span class="Cnonalphakeyword">(</span><span class="Cnonalphakeyword">)</span><span class="Cnonalphakeyword">;</span>
     <span class="Cconstructor">Eliom_predefmod</span><span class="Cnonalphakeyword">.</span><span class="Cconstructor">Xhtml</span><span class="Cnonalphakeyword">.</span>string_input
       <span class="Clabel">~input_type:</span><span class="Cconstructor">`Submit</span> <span class="Clabel">~value:</span><span class="Cnonalphakeyword">(</span><span class="Cstring">"remove "</span>^string_of_int id<span class="Cnonalphakeyword">)</span> <span class="Cnonalphakeyword">(</span><span class="Cnonalphakeyword">)</span><span class="Cnonalphakeyword">)</span>
</pre>
      <p>Here we rewrite the example <code>session_data_example</code>
      using actions
      and named non-attached coservices
      (note the POST coservice for disconnection, much better than the
      previous solution that was using another URL).</p>
*html*)
(************************************************************)
(************ Connection of users, version 3 ****************)
(************************************************************)

(*zap* *)
let session_name = "connect_example3"
(* *zap*)
(* -------------------------------------------------------- *)
(* We create one main service and two (POST) actions        *)
(* (for connection and disconnection)                       *)

let connect_example3 =
  Eliom_services.new_service
    ~path:["action"]
    ~get_params:Eliom_parameters.unit
    ()

let connect_action =
  Eliom_services.new_post_coservice'
    ~name:"connect3"
    ~post_params:(Eliom_parameters.string "login")
    ()

(* As the handler is very simple, we register it now: *)
let disconnect_action =
  Eliom_predefmod.Actions.register_new_post_coservice'
    ~name:"disconnect3"
    ~post_params:Eliom_parameters.unit
    (fun sp () () ->
      Eliom_sessions.close_session (*zap* *) ~session_name (* *zap*) ~sp () >>= fun () ->
      Lwt.return [])


(* -------------------------------------------------------- *)
(* login ang logout boxes:                                  *)

let disconnect_box sp s =
  Eliom_predefmod.Xhtml.post_form disconnect_action sp
    (fun _ -> [p [Eliom_predefmod.Xhtml.string_input
                    ~input_type:`Submit ~value:s ()]]) ()

let login_box sp =
  Eliom_predefmod.Xhtml.post_form connect_action sp
    (fun loginname ->
      [p
         (let l = [pcdata "login: ";
                   Eliom_predefmod.Xhtml.string_input
                     ~input_type:`Text ~name:loginname ()]
         in l)
     ])
    ()


(* -------------------------------------------------------- *)
(* Handler for the "connect_example3" service (main page):    *)

let connect_example3_handler sp () () =
  let sessdat = Eliom_sessions.get_volatile_session_data (*zap* *) ~session_name (* *zap*) ~table:my_table ~sp () in
  return
    (html
       (head (title (pcdata "")) [])
       (body
          (match sessdat with
          | Eliom_sessions.Data name ->
              [p [pcdata ("Hello "^name); br ()];
              disconnect_box sp "Close session"]
          | Eliom_sessions.Data_session_expired
          | Eliom_sessions.No_data -> [login_box sp]
          )))


(* -------------------------------------------------------- *)
(* Handler for connect_action (user logs in):               *)

let connect_action_handler sp () login =
  Eliom_sessions.close_session (*zap* *) ~session_name (* *zap*) ~sp () >>= fun () ->
  Eliom_sessions.set_volatile_session_data (*zap* *) ~session_name (* *zap*) ~table:my_table ~sp login;
  return []


(* -------------------------------------------------------- *)
(* Registration of main services:                           *)

let () =
  Eliom_predefmod.Xhtml.register ~service:connect_example3 connect_example3_handler;
  Eliom_predefmod.Actions.register ~service:connect_action connect_action_handler
(*html*
      <p>$a Tutoeliom.connect_example3 sp <:xmllist< See these pages >> ()$.</p>

     <p>
      Note that actions return a list (here empty).
      $a ~fragment:"p3infofallbacks"
         ~service:tutocur3
         ~sp
         [pcdata "See later for more advanced use"]
         ()
      $
     </p>


     <p>
       That version of the site with connection solves the main problems of
       <a href="#p2sessiondata"><code>sessdata</code></a>:
     </p>
     <ul>
       <li>
         Connection and disconnection stay on the same page,
       </li>
       <li>
         If you want a connection/disconnection form on each page, no need
         to create a version with POST parameters of each service.
       </li>
     </ul>

     <p>
       We'll see later
      $a ~fragment:"p3infofallbacks"
         ~service:tutocur3
         ~sp
         [pcdata "how to display an error message"]
         ()
      $
       if the connection goes wrong, and
      $a ~fragment:"p3persistenceofsessions"
         ~service:tutocur3
         ~sp
         [pcdata "how to have persistent sessions"]
         ()
      $
      (that stay opened even if the server is re-launched).
     </p>



    </div>

    <h3 id="p2detailsonserviceregistration">Details on service registration</h3>
    <div class="encadre sanstitre">
      <ul>
        <li>All services created during initialisation must be registered
        in the public table during the initialisation phase of your module.
        If not, the server will not start (with an error message in the logs).
        Thus, there will always be a service to answer when somebody clicks on
        a link or a form.
        </li>
        <li>Services
         may be registered in the public table after initialisation with
         <code>register</code> only if you add the <code>~sp</code>
           parameter.<br/>
    If you use that for main services,
    you will dynamically create new URLs!
    This may be dangerous as they will disappear if you stop the server.
    Be very careful to re-create these URLs when you relaunch the server,
    otherwise, some external links or bookmarks will be broken!<br/>
    The use of that feature is discouraged for coservices
    without timeout, as such coservices will be available only until the end
    of the server process (and it is not possible to re-create them with the
    same key).
        </li>
        <li>Do not register twice the same service in the public table,
          and do not replace a service
          by a directory (or vice versa). If this happens during the
          initialisation phase, the server won't start.
          If this happens after, it will be ignored (with a warning in the
          logs).
        </li>
        <li>All services (not coservices) must be created in
        a module loaded inside a <code>&lt;site&gt;</code> tag of the
        config file (because they will be attached to a directory).
        Not possible for modules loaded inside <code>&lt;extension&gt;</code>
        or <code>&lt;library&gt;</code>.
        </li>
        <li>GET coservices (whithout POST parameters) can be registered
        only with a main service without GET/POST parameters as fallback.
        But it may be a
      $a ~fragment:"p3preapplied"
         ~service:tutocur3
         ~sp
         [em [pcdata "preapplied"]]
         ()
      $
        service (see below).
        </li>
        <li>Services with POST parameters (main service or coservice)
        can be registered with a (main or co) service without POST
        parameters as fallback.</li>
        <li>The registration of (main) services must be completed before
          the end of the loading of the module. It not possible to launch
          a (Lwt) thread that will register a service later, as
          registering a service needs access to config file
          information (for example the directory of the site).
          If you do this, the server will raise
          $a ~fragment:"EXCEPTIONEliom_function_forbidden_outside_site_loading" ~service:senddoc ~sp [code [pcdata "Eliom_common.Eliom_function_forbidden_outside_site_loading " ]] [version;"Eliom_common.html"]$
          most of the time,
          but you may also get unexpected results (if the thread is executed
          while another site is loaded).
          If you use threads in the initialization phase of your module
          (for example if you need information from a database),
          use $a ~fragment:"VALrun" ~service:senddoc ~sp [code [pcdata "Lwt_unix.run" ]] [version;"Lwt_unix.html"]$ to wait the end of the thread.
        </li>
      </ul>
    </div>


°°


let part3 sp =
°:xmllist°


   <h2>3. More details on services and page generation</h2>
    <div class="onecol">
     <p>
       You now know all Eliom's main concepts. In that part, we'll give
       more details on some aspects that have been seen before:
     </p>
     <ul>
       <li>The different types of output for services</li>
       <li>Timeouts and error handling</li>
       <li>Persistence of sessions</li>
       <li>Advanced forms</li>
     </ul>
    </div>


    <h3 id="p3staticparts">Static parts</h3>
    <div class="onecol">
      <h4>Fully static pages</h4>
      <p>The <code>staticmod</code> extension allows to associate
         to your site a static directory
         where you can put all the static (non generated) parts of your
         web-site (for examples images ans stylesheets).
         See the default config file <code>ocsigen.conf</code> to
         learn how to do that.
         A predefined service can be used to make links to static files.
         Get it using
         <code><span class="Cem">(static_dir ~sp)</span></code>.
         That service takes as string parameter the name of the file.
                <br/>
                For example</p>
         <pre><span class="Cconstructor">Eliom</span>.a
  (static_dir ~sp)
  sp
  [pcdata "download image"]
  "$str:small_logo$"</pre>
          <p>creates this link:
         $a (static_dir ~sp) sp [pcdata "download image"] [small_logo]$
      </p>
      <p>It is now also possible to handle static pages with Eliom, using
      <code>Eliom_predefmod.Files</code> (<a href="#p3eliomfiles">see later</a>).
      </p>
      <!-- h4>Static parts of a page</h4>
      <em>To be available soon</em -->
    </div>



    <h3 id="p3otherkindsofpages">Other kinds of pages</h3>
    <div class="onecol">
    <h4>Sending portions of pages</h4>
    <p>
     The $a ~service:senddoc ~sp [code [pcdata "Eliom_predefmod.Blocks" ]] [version;"Eliom_predefmod.Blocks.html"]$ module allows to register services that
     send portions of pages, of any type that may be contained directly in
     a <code>&lt;body&gt;</code> tag (blocks of xhtml DTD).
     It is useful to create AJAX pages
     (i.e. pages using the <code>XMLHttpRequest</code> Javascript object).
     Note that the service returns a list of blocks.</p>
*html*)
let divpage =
  Eliom_predefmod.Blocks.register_new_service
    ~path:["div"]
    ~get_params:unit
    (fun sp () () ->
      return
        [div [h2 [pcdata "Hallo"];
              p [pcdata "Blablablabla"] ]])
(*html*
     <p>
     The $a ~service:senddoc ~sp [code [pcdata "Eliom_predefmod.SubXhtml" ]] [version;"Eliom_predefmod.SubXhtml.html"]$ module allows to create other modules for
     registering portions of pages of other types.
     For example, $a ~service:senddoc ~sp [code [pcdata "Eliom_predefmod.Blocks" ]] [version;"Eliom_predefmod.Blocks.html"]$
     is defined by:</p>
<pre>
module Blocks = SubXhtml(struct
  type content = Xhtmltypes.body_content
end)
</pre>

    <h4>Redirections</h4>
    <p>
     The $a ~service:senddoc ~sp [code [pcdata "Eliom_predefmod.Redirection" ]] [version;"Eliom_predefmod.Redirection.html"]$ module allows to register HTTP redirections.<br/>  <strong>[New in 1.1.0. For 1.0.0, please see module <code>Eliom_predefmod.Redirections</code>.]</strong><br/>
     If a request is done towards such a service, the server asks the browser
     to retry with another URL. 
    </p>
    <p>
     Such services return a GET service without parameter at all.
     Example:
    </p>
*html*)
let redir1 = Eliom_predefmod.Redirection.register_new_service
    ~options:`Temporary
    ~path:["redir"]
    ~get_params:Eliom_parameters.unit
   (fun sp () () -> Lwt.return coucou)
(*html*
    <p>
     If you want to give parameters to such services, use
     $a ~fragment:"VALpreapply" ~service:senddoc ~sp
           [code [pcdata "Eliom_services.preapply" ]]
           [version;"Eliom_services.html"]$ (see also 
     <a href="#p3preapplied">later in the tutorial</a>).
     Example:
    </p>
 *html*)
let redir = Eliom_predefmod.Redirection.register_new_service
    ~options:`Temporary
    ~path:["redir"]
    ~get_params:(int "o")
   (fun sp o () ->
      Lwt.return
        (Eliom_services.preapply coucou_params (o,(22,"ee"))))
(*html*
      <p>The <code>options</code> parameter may be either
      <code>`Temporary</code> or <code>`Permanent</code>.</p>
      <p>$a Tutoeliom.redir sp <:xmllist< Try it >> 11$.</p>

      <p>Note that the cost of a redirection is one more query and
      one more answer.
      </p>


     <h4 id="p3eliomfiles">Sending files</h4>
      <p>You may want to register a service that will send files.
      To do that, use the $a ~service:senddoc ~sp [code [pcdata "Eliom_predefmod.Files" ]] [version;"Eliom_predefmod.Files.html"]$ module. Example:
      </p>
<pre>
let sendfile =
  Files.register_new_service
    ~path:["sendfile"]
    ~get_params:unit
    (fun _ () () -&gt; return "filename")
</pre>
      <p>Other example, with suffix URL:
      </p>
<pre>
let sendfile2 =
  Files.register_new_service
    ~path:["files"]
    ~get_params:(suffix (all_suffix "filename"))
    (fun _ s () -&gt; return ("<em>path</em>"^(Ocsigen_extensions.string_of_url_path s)))
</pre>
      <p>The extension <code>Staticmod</code> is another way to
       handle static files (see the default
       configuration file for more information).
      </p>

     <h4>Registering services that decide what they want to send</h4>
      <p>You may want to register a service that will send, for instance,
      sometimes
      an xhtml page, sometimes a file, sometimes something else.
      To do that, use the $a ~service:senddoc ~sp [code [pcdata "Eliom_predefmod.Any" ]] [version;"Eliom_predefmod.Any.html"]$ module, together
      with the <code>send</code> function of the module you want
      to use. Example:
      </p>
*html*)
let send_any =
  Eliom_predefmod.Any.register_new_service
    ~path:["sendany"]
    ~get_params:(string "type")
   (fun sp s () ->
     if s = "valid"
     then
       Eliom_predefmod.Xhtml.send sp
         (html
            (head (title (pcdata "")) [])
            (body [p [pcdata
                        "This page has been statically typechecked.
                         If you change the parameter in the URL you will get an unchecked text page"]]))
     else
       Eliom_predefmod.HtmlText.send sp
         "<html><body><p>It is not a valid page. Put type=\"valid\" in the URL to get a typechecked page.</p></body></html>"
   )
(*html*
      <p>
      See $a Tutoeliom.send_any sp <:xmllist< a valid page >> "valid"$,
      and $a Tutoeliom.send_any sp <:xmllist< a non valid page >> "non valid"$.
      </p>
      <p>You may also use $a ~service:senddoc ~sp [code [pcdata "Eliom_predefmod.Any" ]] [version;"Eliom_predefmod.Any.html"]$ to send cookies or to choose a
         different charset than the default
        (default charset is set in configuration file)
         for the page you send. To do that use the optional parameters
          <code>?cookies</code> and <code>?charset</code> of the
         <code>send</code> function.
      </p>
     <h4>Cookies</h4>
     <p>
      A simplest way to set your own cookies on the client is to use
      functions like
      <code>Eliom_predefmod.Xhtml.Cookies.register</code> instead of
      <code>Eliom_predefmod.Xhtml.register</code>.
      The function you register returns a pair containing the page (as usual)
      and a list of cookies, of type <code>Eliom_services.cookie</code>
      defined by:
      </p>
      <pre>
type cookie =
  | Set of string list option * float option * string * string * bool
  | Unset of string list option * string
</pre>
     <p><strong>[New in 1.1.0]</strong> For version 1.0.0, the type 
<code>cookie</code> was slightly different (no secure cookies).</p>
     <p>
     The <code>string list option</code> is a the path for which you want
     to set/unset the cookie (relative to the main directory of your site,
   defined
     in the configuration file). <code>None</code> means for all your site.
     </p>
     <p>
     The <code>float option</code> is a the expiration date
     (Unix timestamp, in seconds since the epoch).
     <code>None</code> means that the cookie will expire when the browser
     will be closed.
     </p>
     <p>
     If the <code>bool</code> is true and the protocol is https, 
     the server will ask the browser to send the cookie only through
     secure connections.
     </p>
     <p>
      You can access the cookies sent by the browser using
      $a ~fragment:"VALget_cookies" ~service:senddoc ~sp [code [pcdata "Eliom_sessions.get_cookies sp" ]] [version;"Eliom_sessions.html"]$.
     </p>
     <p>
      Example:
     </p>
*html*)
let cookiename = "mycookie"

let cookies = new_service ["cookies"] unit ()

let _ = Cookies.register cookies
    (fun sp () () ->
      return
       ((html
         (head (title (pcdata "")) [])
         (body [p [pcdata (try
                             "cookie value: "^
                             (Ocsigen_http_frame.Cookievalues.find
                                cookiename (Eliom_sessions.get_cookies sp))
                           with _ -> "<cookie not set>");
                   br ();
                   a cookies sp [pcdata "send other cookie"] ()]])),
        [Eliom_services.Set (None, None,
                             cookiename,
                             string_of_int (Random.int 100),
                             false)]))
(*html*
      <p>$a Tutoeliom.cookies sp <:xmllist< Try it >> ()$.</p>
    </div>


    <h3 id="p3persistenceofsessions">Persistence of sessions</h3>
    <div class="onecol">
      <p>Tables of sessions (for data or services) are kept in memory,
        and thus will disappear if you close the server process.
        To solve this problem, Ocsigen allows to reload the modules of
        your configuration file without shutting down the server.
        Another solution provided by Eliom is to save session data on hard disk.
      </p>

      <h4>Updating sites without shutting down the server</h4>
      <p>To reload the modules of the configuration file without
       stoping the server, use <code>/etc/init.d/ocsigen reload</code>
       for most of the distributions, or do it manually using:</p>
       <pre>echo reload &gt; /var/run/ocsigen_command</pre>.
      <p>
       Only modules loaded inside <code>&lt;site&gt;</code> or
       <code>&lt;library&gt;</code> will be reloaded.
       Module loaded using <code>&lt;extension&gt;</code> will not.
      </p>
      <p>
        Have a look at the logs to see if all went well during the reload.
        If something went wrong, old services may still be reachable.
      </p>
      <p>
        Note that coservices created with the old modules or
        URLs that have not been masked by new ones
        will still reachable after the update.
      </p>
      <p>
        During the reload, some information of the configuration file
        will not be re-read (for example port numbers, user and group, etc.).
      </p>


      <h4>Persistent data</h4>
      <p>
        Eliom allows to use more persistent data, using the module
        $a ~service:senddoc ~sp [code [pcdata "Ocsipersist" ]] [version;"Ocsipersist.html"]$. (<code>Ocsipersist</code> is needed in
        <code>eliom.cma</code>, thus you need to dynlink it in the
        configuration file before <code>Eliom</code>).
        There are currently two implementations of <code>Ocsipersist</code>:
        <code>ocsipersist-dbm.cma</code> (uses the DBM database) and
        <code>ocsipersist-sqlite.cma</code> (uses the SQLite database,
        and depends on <code>sqlite3.cma</code>).
      </p>
      <p>These modules allow to:
      </p>
      <ul>
        <li>Create persistent references
          (still present after restarting the server),</li>
        <li>Create persistent association tables,</li>
        <li>Set persistent session data (using
        <code>set_persistent_data</code>, see below).</li>
      </ul>
      <p>Note that persistent data are serialized on hard disk using
        OCaml's <code>Marshal</code> module:
      </p>
   <div class="importantwarning">
      <ul>
        <li>It is not possible to serialize closures or services
         (as we are using dynamic linking).</li>
        <li>
 If you ever change the type of serialised data, don't
 forget to delete the database file!
 Or if you really want to keep it, and
 you know what you are doing, you can use the sqlite client to manually
 update the table or a program to create a new sqlite or dbm table
 for the new type.
        </li>
      </ul>
   </div>
   <p>
   Suppose for example that you use <code>get/set_persistent_data</code>
   (see below) to store a (int, string)
 tuple with the user's login credentials.  At this point you stop the
 server, and change the code such that get/set_persistent_data now to store
 a (int, string, string).  Now recompile and restart the server.  If by any
 chance a client with an old cookie reconnects, you get a segfault on the
 server, because of the type change in the data stored in the DB backend ...
   </p>
      <h4>Persistent references</h4>
      <p><code>Ocsipersist</code> allows to create persistent references.
       Here is an example of page with a persistent counter:
      </p>
*html*)
let mystore = Ocsipersist.open_store "eliomexamplestore2"

let count2 =
  let next =
    let cthr = Ocsipersist.make_persistent mystore "countpage" 0 in
    let mutex = Lwt_mutex.create () in
    (fun () ->
      cthr >>=
      (fun c ->
        Lwt_mutex.lock mutex >>= fun () ->
        Ocsipersist.get c >>=
        (fun oldc ->
          let newc = oldc + 1 in
          Ocsipersist.set c newc >>=
          (fun () ->
            Lwt_mutex.unlock mutex;
            return newc))))
  in
  register_new_service
    ~path:["count2"]
    ~get_params:unit
    (fun _ () () ->
      next () >>=
      (fun n ->
        return
         (html
          (head (title (pcdata "counter")) [])
          (body [p [pcdata (string_of_int n)]]))))

(*html*
      <p>
      $a Tutoeliom.count2 sp <:xmllist< See this example here >> ()$.
      </p>
      <h4>Persistent tables</h4>
      <p><code>Ocsipersist</code> also allows to create very basic
       persistent tables. Use them if you don't need complex requests
       on your tables. Otherwise use a database such as <code>PostgreSQL</code>
       or <code>MySQL</code>. Here are the interface you can use:
      </p>
<pre>
type 'value table

val open_table : string -&gt; 'value table

val find : 'value table -&gt; string -&gt; 'value Lwt.t

val add : 'value table -&gt; string -&gt; 'value -&gt; unit Lwt.t

val remove : 'value table -&gt; string -&gt; unit Lwt.t
</pre>

    <p>
      As you can see, all these function are cooperative.
    </p>

      <h4>Persistent session data</h4>
      <p><code>Eliom</code> also implements persistent session tables.
       You can use them instead of memory tables if you don't need
       to register closures.</p>
      <p>The following example is a new version of our site
       with users, with persistent connections.
       (<code>login_box</code>, <code>disconnect_box</code>
       and <code>disconnect_action</code>
       are the same as
      $a ~fragment:"p2actions"
         ~service:tutocur2
         ~sp
         [pcdata "before"]
         ()
      $).
      </p>

*html*)
(************************************************************)
(************ Connection of users, version 4 ****************)
(**************** (persistent sessions) *********************)
(************************************************************)

(*zap* *)
let session_name = "persistent_sessions"
(* *zap*)
let my_persistent_table =
  create_persistent_table "eliom_example_table"

(* -------------------------------------------------------- *)
(* We create one main service and two (POST) actions        *)
(* (for connection and disconnection)                       *)

let persist_session_example =
  Eliom_services.new_service
    ~path:["persist"]
    ~get_params:unit
    ()

let persist_session_connect_action =
  Eliom_services.new_post_coservice'
    ~name:"connect4"
    ~post_params:(string "login")
    ()

(* disconnect_action, login_box and disconnect_box have been
   defined in the section about actions *)(*zap* *)

(* -------------------------------------------------------- *)
(* Actually, no. It's a lie because we don't use the
   same session name :-) *)
(* new disconnect action and box:                           *)

let disconnect_action =
  Eliom_predefmod.Actions.register_new_post_coservice'
    ~name:"disconnect4"
    ~post_params:Eliom_parameters.unit
    (fun sp () () ->
      Eliom_sessions.close_session (*zap* *) ~session_name (* *zap*) ~sp () >>= fun () ->
      return [])

let disconnect_box sp s =
  Eliom_predefmod.Xhtml.post_form disconnect_action sp
    (fun _ -> [p [Eliom_predefmod.Xhtml.string_input
                    ~input_type:`Submit ~value:s ()]]) ()


exception Bad_user

(* -------------------------------------------------------- *)
(* new login box:                                           *)

let login_box sp session_expired action =
  Eliom_predefmod.Xhtml.post_form action sp
    (fun loginname ->
      let l =
        [pcdata "login: ";
         string_input ~input_type:`Text ~name:loginname ()]
      in
      let exnlist = Eliom_sessions.get_exn sp in
      (* If exnlist is not empty, something went wrong
         during an action. We write an error message: *)
      [p (if List.mem Bad_user exnlist
      then (pcdata "Wrong user")::(br ())::l
      else
        if session_expired
        then (pcdata "Session expired")::(br ())::l
        else l)
     ])
    ()

(* *zap*)

(* ----------------------------------------------------------- *)
(* Handler for "persist_session_example" service (main page):  *)

let persist_session_example_handler sp () () =
  Eliom_sessions.get_persistent_session_data (*zap* *) ~session_name (* *zap*)
    ~table:my_persistent_table ~sp () >>= fun sessdat ->
  return
    (html
       (head (title (pcdata "")) [])
       (body
          (match sessdat with
          | Eliom_sessions.Data name ->
              [p [pcdata ("Hello "^name); br ()];
              disconnect_box sp "Close session"]
          | Eliom_sessions.Data_session_expired ->
              [login_box sp true persist_session_connect_action;
               p [em [pcdata "The only user is 'toto'."]]]
          | Eliom_sessions.No_data ->
              [login_box sp false persist_session_connect_action;
               p [em [pcdata "The only user is 'toto'."]]]
          )))


(* ----------------------------------------------------------- *)
(* Handler for persist_session_connect_action (user logs in):  *)

let persist_session_connect_action_handler sp () login =
  Eliom_sessions.close_session (*zap* *) ~session_name (* *zap*) ~sp () >>= fun () ->
  if login = "toto" (* Check user and password :-) *)
  then begin
    Eliom_sessions.set_persistent_session_data (*zap* *) ~session_name (* *zap*) ~table:my_persistent_table ~sp login >>= fun () ->
    return []
  end
  else return [Bad_user]


(* -------------------------------------------------------- *)
(* Registration of main services:                           *)

let () =
  Eliom_predefmod.Xhtml.register
    ~service:persist_session_example
    persist_session_example_handler;
  Eliom_predefmod.Actions.register
    ~service:persist_session_connect_action
    persist_session_connect_action_handler
(*html*
      <p>
      $a Tutoeliom.persist_session_example sp <:xmllist< See this example here >> ()$.
      </p>

      <p>
        As it is not possible to serialize closures, there is no persistent
        session service table. Be very carefull if you use both persistent
        session data tables and service session tables,
        as your session may become inconsistent (use the session service
        table only for volatile services, like coservices with timeouts).
      </p>

    </div>



    <h3 id="p3sessiongroups">[New in 0.99.5 - EXPERIMENTAL] Session groups</h3>
    <div class="onecol">
    <p>The idea is complementary to that of
the "session name".  While the
optional <code>session_name</code> parameter allows for a single session to have
multiple buckets of data associated with it, a session_group parameter
(also optional) allow multiple sessions to be referenced together.
For most uses, the session group is the user name.
It allows to implement features like "close all sessions" for one user
(even those opened on other browsers), or to limit the number of sessions
one user may open at the same time.
    </p>
    <p>Session groups have been suggested by Dario Teixeira and
    introduced in Eliom 0.99.5. Dario explains:
    <em>Consider the following scenario: a user logs in from home using
  a "Remember me on this computer" feature, which sets a (almost)
  no-expiration cookie on his browser and session timeouts of infinity
  on the server.  The user goes on vacation, and while logging from
  a cyber-café, she also sets the "Remember me" option.  Back home
  she realises her mistake, and wishes to do a "global logout", ie,
  closing all existing sessions associated with her user name.
  </em>
    </p>
  *html*)
(************************************************************)
(************ Connection of users, version 5 ****************)
(************************************************************)

(*zap* *)
let session_name = "connect_example5"
(* *zap*)
(* -------------------------------------------------------- *)
(* We create one main service and two (POST) actions        *)
(* (for connection and disconnection)                       *)

let connect_example5 =
  Eliom_services.new_service
    ~path:["groups"]
    ~get_params:Eliom_parameters.unit
    ()

let connect_action =
  Eliom_services.new_post_coservice'
    ~name:"connect5"
    ~post_params:(Eliom_parameters.string "login")
    ()

(* As the handler is very simple, we register it now: *)
let disconnect_action =
  Eliom_predefmod.Actions.register_new_post_coservice'
    ~name:"disconnect5"
    ~post_params:Eliom_parameters.unit
    (fun sp () () ->
      Eliom_sessions.close_session (*zap* *) ~session_name (* *zap*) ~sp () >>= fun () ->
      Lwt.return [])


(* -------------------------------------------------------- *)
(* login ang logout boxes:                                  *)

let disconnect_box sp s =
  Eliom_predefmod.Xhtml.post_form disconnect_action sp
    (fun _ -> [p [Eliom_predefmod.Xhtml.string_input
                    ~input_type:`Submit ~value:s ()]]) ()

let login_box sp =
  Eliom_predefmod.Xhtml.post_form connect_action sp
    (fun loginname ->
      [p
         (let l = [pcdata "login: ";
                   Eliom_predefmod.Xhtml.string_input
                     ~input_type:`Text ~name:loginname ()]
         in l)
     ])
    ()


(* -------------------------------------------------------- *)
(* Handler for the "connect_example5" service (main page):    *)

let connect_example5_handler sp () () =
  let sessdat = Eliom_sessions.get_volatile_data_session_group (*zap* *) ~session_name (* *zap*) ~sp () in
  return
    (html
       (head (title (pcdata "")) [])
       (body
          (match sessdat with
          | Eliom_sessions.Data name ->
              [p [pcdata ("Hello "^name); br ()];
              disconnect_box sp "Close session"]
          | Eliom_sessions.Data_session_expired
          | Eliom_sessions.No_data -> [login_box sp]
          )))


(* -------------------------------------------------------- *)
(* Handler for connect_action (user logs in):               *)

let connect_action_handler sp () login =
  Eliom_sessions.close_session (*zap* *) ~session_name (* *zap*) ~sp () >>= fun () ->
  Eliom_sessions.set_volatile_data_session_group ~set_max:(Some 10) (*zap* *) ~session_name (* *zap*) ~sp login;
  return []


(* -------------------------------------------------------- *)
(* Registration of main services:                           *)

let () =
  Eliom_predefmod.Xhtml.register ~service:connect_example5 connect_example5_handler;
  Eliom_predefmod.Actions.register ~service:connect_action connect_action_handler
(*html*

    <p>
      As we will see later, there are three kinds of sessions
      (services, volatile data and persistent data).
      It is highly recommended to set a group for each of them!
    </p>


    </div>



    <h3 id="p3otherconcepts">Other concepts</h3>
    <div class="onecol">
    <h4 id="p3preapplied">Pre-applied services</h4>
    <p>Services or coservices with GET parameters can be preapplied
     to obtain a service without parameters. Example:
    </p>
    <pre>
let preappl = Eliom_services.preapply coucou_params (3,(4,"cinq"))
    </pre>
    <p>
     It is not possible to register something on a preapplied service,
     but you can use them in links or as fallbacks for coservices.
    </p>

    <h4 id="p3preapplied">Void action <strong>[New in 1.1.0]</strong></h4>
    <p>$a ~fragment:"VALvoid_action" ~service:senddoc ~sp
           [code [pcdata "Eliom_services.void_action" ]]
           [version;"Eliom_services.html"]$:
     is a special non-attached action, with special behaviour:
     it has no parameter at all, even non-attached parameters.
     Use it if you want to make a link to the current page
     without non-attached parameters.
     It is almost equivalent to a POST non-attached coservice without POST
     parameters, on which you register an action that does nothing,
     but you can use it with <code>&lt;a&gt;</code> links, not only forms.
     Example:
    </p>
    <pre>
Eliom_duce.Xhtml.a
  ~service:Eliom_services.void_action
  ~sp
  {{ "cancel" }}
  ()
    </pre>

    <h4 id="p3infofallbacks">Giving information to fallbacks</h4>

    <p>Fallbacks have access to some information about what succeeded before
    they were called. Get this information using
     $a ~fragment:"VALget_exn" ~service:senddoc ~sp [code [pcdata "Eliom_sessions.get_exn sp" ]] [version;"Eliom_sessions.html"]$; That function returns a list of exceptions.
    That list contains $a ~fragment:"EXCEPTIONEliom_Link_too_old" ~service:senddoc ~sp [code [pcdata "Eliom_common.Eliom_Link_too_old" ]] [version;"Eliom_common.html"]$ if the coservice
    was not found, and $a ~fragment:"EXCEPTIONEliom_Service_session_expired" ~service:senddoc ~sp [code [pcdata "Eliom_common.Eliom_Service_session_expired" ]] [version;"Eliom_common.html"]$ if the "service session" has expired.
    </p>
    <p>
    It is also possible to tell actions to send information to the page
    generated after them. Just place exceptions in the list returned by the
    action. These exceptions will also be accessible with
    $a ~fragment:"VALget_exn" ~service:senddoc ~sp [code [pcdata "Eliom_sessions.get_exn" ]] [version;"Eliom_sessions.html"]$.
    Here is the new version of the
          $a ~fragment:"p2actions"
         ~service:tutocur2
         ~sp
         [pcdata "example of session with actions:"]
         ()
      $ by:
    </p>
*html*)
(************************************************************)
(************ Connection of users, version 6 ****************)
(************************************************************)
(*zap* *)
let session_name = "connect_example6"
(* *zap*)
(* -------------------------------------------------------- *)
(* We create one main service and two (POST) actions        *)
(* (for connection and disconnection)                       *)

let connect_example6 =
  Eliom_services.new_service
    ~path:["action2"]
    ~get_params:unit
    ()

let connect_action =
  Eliom_services.new_post_coservice'
    ~name:"connect6"
    ~post_params:(string "login")
    ()

(* new disconnect action and box:                           *)

let disconnect_action =
  Eliom_predefmod.Actions.register_new_post_coservice'
    ~name:"disconnect6"
    ~post_params:Eliom_parameters.unit
    (fun sp () () ->
      Eliom_sessions.close_session (*zap* *) ~session_name (* *zap*) ~sp () >>= fun () ->
      return [])

let disconnect_box sp s =
  Eliom_predefmod.Xhtml.post_form disconnect_action sp
    (fun _ -> [p [Eliom_predefmod.Xhtml.string_input
                    ~input_type:`Submit ~value:s ()]]) ()


exception Bad_user

(* -------------------------------------------------------- *)
(* new login box:                                           *)

let login_box sp session_expired action =
  Eliom_predefmod.Xhtml.post_form action sp
    (fun loginname ->
      let l =
        [pcdata "login: ";
         string_input ~input_type:`Text ~name:loginname ()]
      in
      let exnlist = Eliom_sessions.get_exn sp in
      (* If exnlist is not empty, something went wrong
         during an action. We write an error message: *)
      [p (if List.mem Bad_user exnlist
      then (pcdata "Wrong user")::(br ())::l
      else
        if session_expired
        then (pcdata "Session expired")::(br ())::l
        else l)
     ])
    ()

(* -------------------------------------------------------- *)
(* Handler for the "connect_example6" service (main page):   *)

let connect_example6_handler sp () () =
  let group =
    Eliom_sessions.get_volatile_data_session_group (*zap* *) ~session_name (* *zap*) ~sp ()
  in
  return
    (html
       (head (title (pcdata "")) [])
       (body
          (match group with
          | Eliom_sessions.Data name ->
              [p [pcdata ("Hello "^name); br ()];
              disconnect_box sp "Close session"]
          | Eliom_sessions.Data_session_expired ->
              [login_box sp true connect_action;
               p [em [pcdata "The only user is 'toto'."]]]
          | Eliom_sessions.No_data ->
              [login_box sp false connect_action;
               p [em [pcdata "The only user is 'toto'."]]]
          )))

(* -------------------------------------------------------- *)
(* New handler for connect_action (user logs in):           *)

let connect_action_handler sp () login =
  Eliom_sessions.close_session (*zap* *) ~session_name (* *zap*) ~sp () >>= fun () ->
  if login = "toto" (* Check user and password :-) *)
  then begin
    Eliom_sessions.set_volatile_data_session_group ~set_max:(Some 10) (*zap* *) ~session_name (* *zap*) ~sp login;
    return []
  end
  else return [Bad_user]

(* -------------------------------------------------------- *)
(* Registration of main services:                           *)

let () =
  Eliom_predefmod.Xhtml.register ~service:connect_example6 connect_example6_handler;
  Eliom_predefmod.Actions.register ~service:connect_action connect_action_handler

(*html*
      <p>
      $a Tutoeliom.connect_example6 sp <:xmllist< See this example here >> ()$.
      </p>
      <p>
        If the actions raises an exception (with $a ~fragment:"VALfail" ~service:senddoc ~sp [code [pcdata "Lwt.fail" ]] [version;"Lwt.html"]$),
        the server will send an error 500 (like for any other service).
        Think about catching the exceptions and put them in the list
        if they correspond to usual cases you want to handle while
        generating the page after the action.
      </p>



     <h4>Disposable coservices</h4>
      <p>It is possible to set a limit to the number of uses of
      (attached or non-attached) coservices. Just give the maximum number
      of uses with the optional <code>?max_use</code> parameter while
      creating your coservices. Example
      </p>
*html*)
let disposable = new_service ["disposable"] unit ()

let _ = register disposable
    (fun sp () () ->
      let disp_coservice =
        new_coservice ~max_use:2 ~fallback:disposable ~get_params:unit ()
      in
      register_for_session sp disp_coservice
        (fun sp () () ->
          return
            (html
              (head (title (pcdata "")) [])
              (body [p [pcdata "I am a disposable coservice";
                        br ();
                        a disp_coservice sp [pcdata "Try me once again"] ()]]))
        );
      return
        (html
          (head (title (pcdata "")) [])
          (body [p [(if List.mem
                          Eliom_common.Eliom_Link_too_old
                          (Eliom_sessions.get_exn sp)
                    then pcdata "Your link was outdated. I am the fallback. I just created a new disposable coservice. You can use it only twice."
                    else
                    pcdata "I just created a disposable coservice. You can use it only twice.");
                    br ();
                    a disp_coservice sp [pcdata "Try it!"] ()]])))
(*html*
      <p>$a Tutoeliom.disposable sp <:xmllist< Try it >> ()$.</p>
     <h4>Timeout for sessions</h4>
      <p>The default timeout for sessions in one hour. Sessions will be
       automatically closed after that amount of time of inactivity
       from the user.
       You can change that value for your whole site during initialisation
       using:</p>
<pre>
Eliom_sessions.set_global_volatile_timeout (Some 7200.)
</pre>
      <p>Here 7200 seconds. <code>None</code> means no timeout.</p>
      <p>
       You can change that value for your whole site after initialisation
       using:</p>
<pre>
Eliom_sessions.set_global_volatile_timeout ~sp (Some 7200.)
</pre>
      <p>
       You can change that value for one user only using:</p>
<pre>
Eliom_sessions.set_volatile_session_timeout ~sp (Some 7200.)
</pre>
      <p>
      Note that there is also a possibility to change the default value
      for Eliom in the configuration file like this:</p>
<pre>
    &lt;extension module="<em>path_to</em>/eliom.cma"&gt;
      &lt;timeout value="7200"/&gt;
    &lt;/extension&gt;
</pre>
     <p><code>value="infinity"</code> means no timeout.</p>
     <p>Warning: that default may be overriden by each site using
        $a ~fragment:"VALset_global_volatile_timeout" ~service:senddoc ~sp [code [pcdata "Eliom_sessions.set_global_volatile_timeout" ]] [version;"Eliom_sessions.html"]$ or
        $a ~fragment:"VALset_default_volatile_timeout" ~service:senddoc ~sp [code [pcdata "Eliom_sessions.set_default_volatile_timeout" ]] [version;"Eliom_sessions.html"]$.
        If you want your user to be able to set the default in the
        configuration file for your site (between <code>&lt;site&gt;</code>
        and <code>&lt;/site&gt;</code>), you must parse the configuration
        ($a ~fragment:"VALget_config" ~service:senddoc ~sp [code [pcdata "Eliom_sessions.get_config ()" ]] [version;"Eliom_sessions.html"]$ function, see below).
     </p>



     <h4>Timeout for coservices</h4>
      <p>It is also possible to put timeouts on coservices using
      the optional parameter <code>?timeout</code> of functions
      <code>new_coservice</code>,
      <code>new_coservice'</code>, etc.
     Note that session coservices cannot survive after the end of the session.
     Use this if you don't want your coservice to be available during all the
     session duration. For example if your coservice is here to show the
     results of a search, you probably want it to be available only for
     a short time. The following example shows a coservice with timeout
     registered in the session table.
     </p>
*html*)
let timeout = new_service ["timeout"] unit ()

let _ =
  let page sp () () =
    let timeoutcoserv =
      register_new_coservice_for_session
        ~sp ~fallback:timeout ~get_params:unit ~timeout:5.
        (fun _ _ _ ->
           return
             (html
               (head (title (pcdata "Coservices with timeouts")) [])
               (body [p
                 [pcdata "I am a coservice with timeout."; br ();
                  pcdata "Try to reload the page!"; br ();
                  pcdata "I will disappear after 5 seconds of inactivity." ];
                 ])))
    in
    return
      (html
        (head (title (pcdata "Coservices with timeouts")) [])
        (body [p
          [pcdata "I just created a coservice with 5 seconds timeout."; br ();
           a timeoutcoserv sp [pcdata "Try it"] (); ];
          ]))
  in
  register timeout page
(*html*
      <p>
      $a Tutoeliom.timeout sp <:xmllist< See this example here >> ()$.
      </p>
     <h4>Registering coservices in public table during session</h4>
     <p>If you want to register coservices in the
     public table during a session, (that is, after the initialisation
     phase of your module), you must add the optional <code>~sp</code>
     parameter to the <code>register</code> function.
     Remember that using <code>register</code> without <code>~sp</code>
     is possible only during initialisation!
     </p>
     <p>
     We recommend to put a timeout on such coservices, otherwise, they
     will be available until the end of the server process, and it will not be
     possible to re-create them when the server is relaunched.
     </p>
     <p>
     The following example is a translation of the previous one using
     the public table:
     </p>
*html*)
let publiccoduringsess = new_service ["publiccoduringsess"] unit ()

let _ =
  let page sp () () =
    let timeoutcoserv =
      register_new_coservice
        ~sp ~fallback:publiccoduringsess ~get_params:unit ~timeout:5.
        (fun _ _ _ ->
           return
             (html
               (head (title (pcdata "Coservices with timeouts")) [])
               (body [p
                 [pcdata "I am a public coservice with timeout."; br ();
                  pcdata "I will disappear after 5 seconds of inactivity." ];
                 ])))
    in
    return
      (html
        (head (title (pcdata "Public coservices with timeouts")) [])
        (body [p
          [pcdata "I just created a public coservice with 5 seconds timeout."; br ();
           a timeoutcoserv sp [pcdata "Try it"] (); ];
          ]))
  in
  register publiccoduringsess page
(*html*
      <p>
      $a Tutoeliom.publiccoduringsess sp <:xmllist< See this example here >> ()$.
      </p>
     <h4>Defining an exception handler for the whole site</h4>
     <p>When an exception is raised during the generation of a page,
     or when the page has not been found or has wrong parameters,
     an HTTP error 500 or 404 is sent to the client. You may want to
     catch these exceptions to print your own error page.
     Do this using $a ~fragment:"VALset_exn_handler" ~service:senddoc ~sp [code [pcdata "Eliom_services.set_exn_handler" ]] [version;"Eliom_services.html"]$.
     Here is the handler used by this tutorial:
     </p>
*html*)
let _ = Eliom_services.set_exn_handler
   (fun sp e -> match e with
    | Eliom_common.Eliom_404 ->
        Eliom_predefmod.Xhtml.send ~code:404 ~sp
          (html
             (head (title (pcdata "")) [])
             (body [h1 [pcdata "Eliom tutorial"];
                    p [pcdata "Page not found"]]))
    | Eliom_common.Eliom_Wrong_parameter ->
        Eliom_predefmod.Xhtml.send ~sp
          (html
             (head (title (pcdata "")) [])
             (body [h1 [pcdata "Eliom tutorial"];
                    p [pcdata "Wrong parameters"]]))
    | e -> fail e)
(*html*
     <h4>Giving configuration options to your sites</h4>
      <p>You can add your own options in the configuration
       file for your Web site. For example:</p>
<pre>
    &lt;eliom module="<em>path_to</em>/yourmodule.cmo"&gt;
      &lt;youroptions&gt; ...
    &lt;/eliom&gt;
</pre>
      <p>
       Use $a ~fragment:"VALget_config" ~service:senddoc ~sp [code [pcdata "Eliom_sessions.get_config ()" ]] [version;"Eliom_sessions.html"]$ during the initialization
       of your module to get the data between
       <code>&lt;eliom&gt;</code> and <code>&lt;/eliom&gt;</code>.
       Warning: parsing these data is very basic for now.
       That feature will be improved in the future.
      </p>
     <h4>More about sessions</h4>
      <p>By default, Eliom is using three cookies :</p>
      <ul>
        <li>One for session services,</li>
        <li>one for volatile session data,</li>
        <li>one for persistent session data.</li>
      </ul>
      <p>They correspond to three different sessions (opened only if needed).
   <span class="Cem">$a ~fragment:"VALclose_session" ~service:senddoc ~sp [code [pcdata "Eliom_sessions.close_session" ]] [version;"Eliom_sessions.html"]$</span>
       closes all three sessions, but you may want to desynchronize
       the three sessions by using
   <span class="Cem">$a ~fragment:"VALclose_persistent_session" ~service:senddoc ~sp [code [pcdata "Eliom_sessions.close_persistent_session" ]] [version;"Eliom_sessions.html"]$</span> (persistent session),
   <span class="Cem">$a ~fragment:"VALclose_service_session" ~service:senddoc ~sp [code [pcdata "Eliom_sessions.close_service_session" ]] [version;"Eliom_sessions.html"]$</span> (session services), or
   <span class="Cem">$a ~fragment:"VALclose_data_session" ~service:senddoc ~sp [code [pcdata "Eliom_sessions.close_data_session" ]] [version;"Eliom_sessions.html"]$</span> (volatile data session).
     There is also
   <span class="Cem">$a ~fragment:"VALclose_volatile_session" ~service:senddoc ~sp [code [pcdata "Eliom_sessions.close_volatile_session" ]] [version;"Eliom_sessions.html"]$</span> for both volatile data session and session services.
       The module $a ~service:senddoc ~sp [code [pcdata "Eliom_sessions" ]] [version;"Eliom_sessions.html"]$ also contains functions for setting timeouts or expiration dates for cookies for each kind of session.
      </p>
      <p>If you need more sessions (for example several different data sessions)
         for the same site, you can give a name to your sessions by giving
         the optional parameter <code>?session_name</code> to functions like
     <span class="Cem">$a ~fragment:"VALclose_data_session" ~service:senddoc ~sp [code [pcdata "Eliom_sessions.close_data_session" ]] [version;"Eliom_sessions.html"]$</span>,
     <span class="Cem">$a ~fragment:"VALregister_for_session" ~service:senddoc ~sp [code [pcdata "register_for_session" ]] [version;"Eliom_mkreg.ELIOMREGSIG1.html"]$</span>, or
      $a ~fragment:"VALget_volatile_session_data" ~service:senddoc ~sp [code [pcdata "Eliom_sessions.get_volatile_session_data" ]] [version;"Eliom_sessions.html"]$.
       Note that this tutorial has been implemented using this feature,
       even if it has been hidden for the sake of simplicity.
       That's how the different examples of sessions in this tutorial are
       independant.
      </p>
      <h4>Secure services <strong>[New in 1.1.0]</strong></h4>
      <p>You may want to impose HTTPS for some of your services.
       To do that, use the optional parameter <code>~https:true</code>
       while creating your service.
      </p>
      <p>It is also possible to require http or https while creating a link or
      a form (using the optional parameter <code>~https:true</code>).
      But it is never possible to make an http link towards an https service,
      even if you request it.
      </p>
      <p>Warning: if the protocol needs to be changed (from http to https 
       or vice versa), Eliom will generate absolute URLs.
       The host name and port numbers are guessed from the IP and the 
       configuration by default, but it is recommended to specify them
       in the configuration file. For example:
      </p>
      <pre>&lt;host name="*.org" defaulthostname="www.mywebsite.org" defaulthttpport="8080" defaulthttpsport="4433"&gt; ... &lt;/host&gt;</pre>

      <h4>Secure sessions <strong>[New in 1.1.0]</strong></h4>
      <p>For security reasons, Eliom does not use the same cookies in
        https and http. Secure sessions are using secure cookies
        (i.e. Ocsigen will ask the browsers to send the cookie only if
        the protocol is secure). Thus it is not possible to access
        secure session if the user is using http. If the user is using
        https, Eliom will save data and services in secure session. But
        it is possible to access unsecure session data and to register
        unsecure session services using the optional parameter
        <code>~secure:false</code> when calling functions like
        <code>Eliom_sessions.set_volatile_session_data</code>,
        <code>Eliom_sessions.get_persistent_session_data</code>,
        <code>Eliom_predefmod.Xhtml.register_for_session</code>, etc.
      </p>
    </div>





    <h3 id="p3advancedformsandparameters">Advanced forms and parameters</h3>

    <div class="onecol">
      <p>This section shows more advanced use of page parameters and
      corresponding forms.</p>
      <h4>Parsing parameters using regular expressions</h4>
      <p>
        Eliom_parameters.regexp allows to parse page parameters using (Perl-compatible)
        regular expressions. We use the module <code>Netstring_pcre</code>,
        from <em>OCamlnet</em>. See the documentation about OCamlnet
        for more information.
        The following example shows a service that accepts only parameters
        values enclosed between <code>[</code> and <code>]</code>:
      </p>
<pre>
<span class="Clet">let</span> r <span class="Cnonalphakeyword">=</span> <span class="Cconstructor">Netstring_pcre</span><span class="Cnonalphakeyword">.</span>regexp <span class="Cstring">"\\\\[(.*)\\\\]"</span>

<span class="Clet">let</span> regexp <span class="Cnonalphakeyword">=</span>
  <span class="Cconstructor">Eliom_predefmod</span><span class="Cnonalphakeyword">.</span><span class="Cconstructor">Xhtml</span><span class="Cnonalphakeyword">.</span>register_new_service
    <span class="Clabel">~path:</span><span class="Cnonalphakeyword">[</span><span class="Cstring">"regexp"</span><span class="Cnonalphakeyword">]</span>
    <span class="Clabel">~get_params:</span><span class="Cnonalphakeyword">(</span>regexp r <span class="Cstring">"$$1"</span> <span class="Cstring">"myparam"</span><span class="Cnonalphakeyword">)</span>
    <span class="Cnonalphakeyword">(</span><span class="Cfun">fun</span> <span class="Cnonalphakeyword">_</span> g <span class="Cnonalphakeyword">(</span><span class="Cnonalphakeyword">)</span> <span class="Cnonalphakeyword">-&gt;</span>
      return
        <span class="Cnonalphakeyword">(</span>html
           <span class="Cnonalphakeyword">(</span>head <span class="Cnonalphakeyword">(</span>title <span class="Cnonalphakeyword">(</span>pcdata <span class="Cstring">""</span><span class="Cnonalphakeyword">)</span><span class="Cnonalphakeyword">)</span> <span class="Cnonalphakeyword">[</span><span class="Cnonalphakeyword">]</span><span class="Cnonalphakeyword">)</span>
           <span class="Cnonalphakeyword">(</span>body <span class="Cnonalphakeyword">[</span>p <span class="Cnonalphakeyword">[</span>pcdata g<span class="Cnonalphakeyword">]</span><span class="Cnonalphakeyword">]</span><span class="Cnonalphakeyword">)</span><span class="Cnonalphakeyword">)</span><span class="Cnonalphakeyword">)</span>
</pre>
*html*)
(*zap* *)
let myregexp = Netstring_pcre.regexp "\\[(.*)\\]"

let regexpserv =
  Eliom_predefmod.Xhtml.register_new_service
    ~path:["regexp"]
    ~get_params:(regexp myregexp "$1" "myparam")
    (fun _ g () ->
      return
        (html
           (head (title (pcdata "")) [])
           (body [p [pcdata g]])))
(* *zap*)
(*html*

      <p>$a Tutoeliom.regexpserv sp <:xmllist< Try it >> "[toto]"$.</p>

      <h4>Boolean checkboxes</h4>
      <p>Page may take parameter of type <code>bool</code>.
         A possible use of this type is in a form
         with <em>boolean checkboxes</em>, as in the example below:
      </p>
*html*)
(* Form with bool checkbox: *)
let bool_params = register_new_service
    ~path:["bool"]
    ~get_params:(bool "case")
  (fun _ case () ->
    return
    << <html>
         <head><title></title></head>
         <body>
         <p>
           $pcdata (if case then "checked" else "not checked")$
         </p>
         </body>
       </html> >>)

let create_form_bool casename =
    <:xmllist< <p>check? $bool_checkbox ~name:casename ()$ <br/>
      $string_input ~input_type:`Submit ~value:"Click" ()$</p> >>

let form_bool = register_new_service ["formbool"] unit
  (fun sp () () ->
     let f = get_form bool_params sp create_form_bool in
     return
     << <html>
          <head><title></title></head>
          <body> $f$ </body>
        </html> >>)


(*html*
      <p>$a Tutoeliom.form_bool sp <:xmllist< Try it >> ()$.</p>

      <p><em>Important warning:</em>
        As you can see, browsers do not send any value
        for unchecked boxes! An unchecked box is equivalent to no parameter
        at all! Thus it is not possible to distinguish between a service
        taking a boolean and a service taking no parameter at all
        (if they share the same URL).
        In Eliom <em>services are tried in order of registration!</em>
        The first matching service will answer.
      </p>

      <p>Other types similar to bool:</p>
      <ul>
       <li>
        $a ~fragment:"VALopt" ~service:senddoc ~sp [code [pcdata "Eliom_parameters.opt" ]] [version;"Eliom_parameters.html"]$ (page taking an optional parameter),</li>
       <li>
        $a ~fragment:"VALsum" ~service:senddoc ~sp [code [pcdata "Eliom_parameters.sum" ]] [version;"Eliom_parameters.html"]$ (either a parameter or another).</li>
      </ul>
      <p>
        See the interface
        $a ~service:senddoc ~sp [pcdata "here"]
          [version;"Eliom_parameters.html"]$.
      </p>

      <h4>Type <code>set</code></h4>
      <p>Page may take several parameters of the same name.
      It is useful when you want to create a form with a variable number
      of fields.
      To do that with Eliom, use the type $a ~fragment:"VALset" ~service:senddoc ~sp [code [pcdata "Eliom_parameters.set" ]] [version;"Eliom_parameters.html"]$.
      For example <code>set int "val"</code> means that the page will take
      zero, one or several parameters of name <code>"val"</code>,
      all of type <code>int</code>.
      The function you register will receive the parameters in a list.
      Example:
      </p>
*html*)

let set = register_new_service
    ~path:["set"]
    ~get_params:(set string "s")
  (fun _ l () ->
    let ll =
      List.map
        (fun s -> << <strong>$str:s$ </strong> >>) l
    in
    return
    << <html>
         <head><title></title></head>
         <body>
         <p>
           You sent:
           $list:ll$
         </p>
         </body>
       </html> >>)
(*html*

   <p>These parameters may come from several kinds of widgets in forms.
   Here is an example of a form with several checkboxes, all sharing the same
   name, but with different values:
   </p>

*html*)

(* form to set *)
let setform = register_new_service
    ~path:["setform"]
    ~get_params:unit
    (fun sp () () ->
      return
        (html
           (head (title (pcdata "")) [])
           (body [h1 [pcdata "Set Form"];
                  get_form set sp
                    (fun n ->
                      [p [pcdata "Form to set: ";
                          string_checkbox ~name:n ~value:"box1" ();
                          string_checkbox
                            ~name:n ~value:"box2" ~checked:true ();
                          string_checkbox ~name:n ~value:"box3" ();
                          string_checkbox ~name:n ~value:"box4" ();
                          string_input ~input_type:`Submit ~value:"Click" ()]])
                ])))
(*html*
      <p>$a Tutoeliom.setform sp <:xmllist< Try it >> ()$.</p>

      <p>Once again, note that there is no difference between an empty
      set or no parameter at all. If you register a service without parameters
      and a service with a set of parameters on the same URL, the firstly
      registered service that matches will answer.
      </p>
      <h4>Select</h4>
      <p>Here is an example of a select box.</p>
*html*)
let select_example_result = register_new_service
    ~path:["select"]
    ~get_params:(string "s")
    (fun sp g () ->
      return
        (html
           (head (title (pcdata "")) [])
           (body [p [pcdata "You selected: ";
                     strong [pcdata g]]])))

let create_select_form =
  (fun select_name ->
    [p [pcdata "Select something: ";
        Eliom_predefmod.Xhtml.string_select ~name:select_name
          (Eliom_predefmod.Xhtml.Option ([] (* attributes *),
                                        "Bob" (* value *),
                                        None (* Content, if different from value *),
                                        false (* not selected *))) (* first line *)
          [Eliom_predefmod.Xhtml.Option ([], "Marc", None, false);
          (Eliom_predefmod.Xhtml.Optgroup
          ([],
           "Girls",
           ([], "Karin", None, false),
           [([a_disabled `Disabled], "Juliette", None, false);
            ([], "Alice", None, true);
            ([], "Germaine", Some (pcdata "Bob's mother"), false)]))]
          ;
        Eliom_predefmod.Xhtml.string_input ~input_type:`Submit ~value:"Send" ()]])

let select_example = register_new_service ["select"] unit
  (fun sp () () ->
     let f =
       Eliom_predefmod.Xhtml.get_form
         select_example_result sp create_select_form
     in
     return
       (html
         (head (title (pcdata "")) [])
         (body [f])))
(*html*
      <p>$a Tutoeliom.select_example sp <:xmllist< Try it >> ()$.</p>
     <p>To do "multiple" select boxes, use functions like
   $a ~fragment:"VALstring_multiple_select" ~service:senddoc ~sp [code [pcdata "Eliom_predefmod.Xhtml.string_multiple_select" ]] [version;"Eliom_predefmod.XHTMLFORMSSIG.html"]$.
   As you can see in the type, the service must be declared with parameters
   of type $a ~fragment:"VALset" ~service:senddoc ~sp [code [pcdata "set" ]] [version;"Eliom_parameters.html"]$.
     </p>




      <h4>Clickable images</h4>
      <p>Here is an example of clickable image.
      You receive the coordinates the user clicked on.
      </p>
*html*)
let coord = register_new_service
    ~path:["coord"]
    ~get_params:(coordinates "coord")
  (fun _ c () ->
    return
  << <html>
       <head><title></title></head>
       <body>
       <p>
         You clicked on coordinates:
         ($str:(string_of_int c.abscissa)$, $str:(string_of_int c.ordinate)$)
       </p>
       </body>
     </html> >>)

(* form to image *)
let imageform = register_new_service
    ~path:["imageform"]
    ~get_params:unit
    (fun sp () () ->
      return
        (html
           (head (title (pcdata "")) [])
           (body [h1 [pcdata "Image Form"];
                  get_form coord sp
                    (fun n ->
                      [p [image_input
                            ~src:(make_uri ~service:(static_dir sp) ~sp ["ocsigen5.png"])
                            ~name:n
                            ()]])
                ])))
(*html*
      <p>$a Tutoeliom.imageform sp <:xmllist< Try it >> ()$.</p>
     <p>You may also send a value with the coordinates:</p>
*html*)
let coord2 = register_new_service
    ~path:["coord2"]
    ~get_params:(int_coordinates "coord")
  (fun _ (i, c) () ->
    return
  << <html>
       <head><title></title></head>
       <body>
       <p>
         You clicked on coordinates:
         ($str:(string_of_int c.abscissa)$, $str:(string_of_int c.ordinate)$)
       </p>
       </body>
     </html> >>)

(* form to image *)
let imageform2 = register_new_service
    ~path:["imageform2"]
    ~get_params:unit
    (fun sp () () ->
      return
        (html
           (head (title (pcdata "")) [])
           (body [h1 [pcdata "Image Form"];
                  get_form coord2 sp
                    (fun n ->
                      [p [int_image_input
                            ~src:(make_uri ~service:(static_dir sp) ~sp ["ocsigen5.png"])
                            ~name:n
                            ~value:3
                            ()]])
                ])))

(*html*
      <p>$a Tutoeliom.imageform2 sp <:xmllist< Try it >> ()$.</p>


      <h4>Type <code>list</code></h4>
        <p>Another way (than $a ~fragment:"VALset" ~service:senddoc ~sp [code [pcdata "Eliom_parameters.set" ]] [version;"Eliom_parameters.html"]$) to do variable length forms
        is to use indexed lists (using $a ~fragment:"VALlist" ~service:senddoc ~sp [code [pcdata "Eliom_parameters.list" ]] [version;"Eliom_parameters.html"]$).
        The use of that feature is a bit more complex than <code>set</code>
        and still experimental.
        Here is an example of service taking an indexed list as parameter:
        </p>
*html*)

(* lists *)
let coucou_list = register_new_service
    ~path:["coucou"]
    ~get_params:(list "a" (string "str"))
  (fun _ l () ->
    let ll =
      List.map (fun s -> << <strong>$str:s$</strong> >>) l in
    return
      << <html>
           <head><title></title></head>
           <body>
           <p>
             You sent:
             $list:ll$
           </p>
           </body>
         </html> >>)
(*html*
      <p>
   Here is an example of link towards this service:
   $a Tutoeliom.coucou_list sp
     [pcdata "coucou?a.str[0]=toto&a.str[1]=titi"] ["toto"; "titi"]$.
      </p>
   <p>
   <em>Warning:</em>
   As for sets or bools,
   if a request has no parameter, it will be considered as the empty list.
   Services are tried in order of registration.
   </p>

   <p>
   As you see, the names of each list element is built from the name
   of the list, the name of the list element, and an index.
   To spare you creating yourself these names, Eliom provides you an iterator
   to create them.
   </p>

*html*)
(*zap* Note:
   Actually almost all services will be overwritten by new versions,
   but not those with user_type parameters for example
   (because the type description contains functions)
 *zap*)

(* Form with list: *)
let create_listform f =
  (* Here, f.it is an iterator like List.map,
     but it must be applied to a function taking 2 arguments
     (unlike 1 in map), the first one being the name of the parameter,
     and the second one the element of list.
     The last parameter of f.it is the code that must be appended at the
     end of the list created
   *)
  f.it (fun stringname v ->
    <:xmllist< <p>Write the value for $str:v$:
      $string_input ~input_type:`Text ~name:stringname ()$ </p> >>)
    ["one";"two";"three";"four"]
    <:xmllist< <p>$string_input ~input_type:`Submit ~value:"Click" ()$</p> >>

let listform = register_new_service ["listform"] unit
  (fun sp () () ->
     let f = get_form coucou_list sp create_listform in
     return
      << <html>
           <head><title></title></head>
           <body> $f$ </body>
         </html> >>)

(*html*

      <p>$a Tutoeliom.listform sp <:xmllist< Try it >> ()$.</p>

      <p>
      <em>Important warning:</em>
      As we have seen in the section about boolean (or optional)
      parameters, it is not possible to distinguish between a boolean
      with value "false", and no parameter at all.
      This causes problems if you create a list of boolean or optional
      values, as it is not possible to know the length of the list.
      In that case, Eliom always takes the shortest possible list.
      </p>

      <h4>Forms and suffixes</h4>

      <p>Service with "suffix" URLs have an equivalent version with
      usual parameters, allowing to create forms towards such services.
      Example:
      </p>
*html*)
(* Form for service with suffix: *)
let create_suffixform ((suff, endsuff),i) =
    <:xmllist< <p>Write the suffix:
      $int_input ~input_type:`Text ~name:suff ()$ <br/>
      Write a string: $user_type_input
         ~input_type:`Text ~name:endsuff Ocsigen_extensions.string_of_url_path$ <br/>
      Write an int: $int_input ~input_type:`Text ~name:i ()$ <br/>
      $string_input ~input_type:`Submit ~value:"Click" ()$</p> >>

let suffixform = register_new_service ["suffixform"] unit
  (fun sp () () ->
     let f = get_form isuffix sp create_suffixform in
     return
      << <html>
           <head><title></title></head>
           <body> $f$ </body>
         </html> >>)

(*html*

      <p>$a Tutoeliom.suffixform sp <:xmllist< Try it >> ()$.</p>

      <h4>Uploading files</h4>

      <p>The $a ~fragment:"VALfile" ~service:senddoc ~sp [code [pcdata "Eliom_parameters.file" ]] [version;"Eliom_parameters.html"]$ parameter type allows to send files in your
       request. The service gets something of type
       $a ~fragment:"TYPEfile_info" ~service:senddoc ~sp [code [pcdata "Ocsigen_extensions.file_info" ]] [version;"Ocsigen_extensions.html"]$. You can extract information
       using this using these functions (from $a ~service:senddoc ~sp [code [pcdata "Eliom_sessions" ]] [version;"Eliom_sessions.html"]$):
      </p>
<pre>
val get_tmp_filename : Ocsigen_extensions.file_info -&gt; string
val get_filesize : Ocsigen_extensions.file_info -&gt; int64
val get_original_filename : Ocsigen_extensions.file_info -&gt; string
</pre>
      <p>$a ~fragment:"VALget_tmp_filename" ~service:senddoc ~sp [code [pcdata "Eliom_sessions.get_tmp_filename" ]] [version;"Eliom_sessions.html"]$ allows to know the actual name
       of the uploaded file on the hard disk.
        $a ~fragment:"VALget_original_filename" ~service:senddoc ~sp [code [pcdata "Eliom_sessions.get_original_filename" ]] [version;"Eliom_sessions.html"]$ gives the original filename.</p>
      <p>To make possible the upload of files, you must configure a
      directory for uploaded files in Ocsigen's configuration file.
      For example:
      </p>
<pre>
  &lt;uploaddir&gt;/tmp&lt;/uploaddir&gt;
</pre>
      <p>Files are kept in this directory only during the request.
       Then they are automatically cancelled.
       Thus your services must copy them
       somewhere else themselves if they want to keep them.
       In the following example, we create a new hard link to the file
       to keep it (the destination must be on the same partition of the disk).
      </p>
*html*)
let upload = new_service
    ~path:["upload"]
    ~get_params:unit
    ()

let upload2 = register_new_post_service
   ~fallback:upload
   ~post_params:(file "file")
    (fun _ () file ->
      let to_display =
        let newname = "/tmp/thefile" in
        (try
          Unix.unlink newname;
        with _ -> ());
        Unix.link (Eliom_sessions.get_tmp_filename file) newname;
        let fd_in = open_in newname in
        try
          let line = input_line fd_in in close_in fd_in; line (*end*)
        with End_of_file -> close_in fd_in; "vide"
      in
      return
        (html
           (head (title (pcdata "Upload")) [])
           (body [h1 [pcdata to_display]])))


let uploadform = register upload
    (fun sp () () ->
      let f =
        (post_form upload2 sp
           (fun file ->
             [p [file_input ~name:file ();
                 br ();
                 string_input ~input_type:`Submit ~value:"Send" ()
               ]]) ()) in
      return
        (html
           (head (title (pcdata "form")) [])
           (body [f])))


(*html*
      <p>$a Tutoeliom.upload sp <:xmllist< Try it >> ()$
      (warning: uploading on ocsigen.org is forbidden).</p>


    </div>



    <h3 id="p3predefinedconstructs">Predefined constructs</h3>
    <div class="onecol">
      <h4>Images, CSS, Javascript</h4>
      <p>
      To include an image, simply use the function $a ~fragment:"VALimg" ~service:senddoc ~sp [code [pcdata "XHTML.M.img" ]] [version;"XHTML.M.html"]$:
      </p>
      <pre>img <span class="Clabel">~alt:</span>"Ocsigen"
    <span class="Clabel">~src:</span>(<span class="Cem">Eliom_predefmod.Xhtml.make_uri</span> ~service:senddoc ~sp [<span class="Cstring">"ocsigen1024.jpg"</span>])
    ()</pre>
      <p>The function <span class="Cem">$a ~fragment:"VALmake_uri" ~service:(static_dir sp) ~sp [code [pcdata "Eliom_predefmod.Xhtml.make_uri" ]] [version;"Eliom_predefmod.XHTMLFORMSSIG.html"]$</span>
        creates the relative URL string from current URL (in <code>sp</code>)
        (see above) to the URL of the image in the static directory
        configured in the configuration file.
      </p>
      <p>To simplify the creation of <code>&lt;link&gt;</code> tags
      for CSS or <code>&lt;script&gt;</code> tags for Javascript,
        use the following functions:</p>
      <pre><span class="Cem">css_link</span> ~uri:(make_uri ~service:(static_dir sp) ~sp [<span class="Cstring">"style.css"</span>]) ()</pre>
      <pre><span class="Cem">js_script</span> ~uri:(make_uri ~service:(static_dir sp) ~sp [<span class="Cstring">"funs.js"</span>]) ()</pre>
      <h4>Basic menus</h4>
      <p>
      To make a menu on your web page, you can use the function
          <span class="Cem">$a ~fragment:"VALmenu" ~service:senddoc ~sp [code [pcdata "Eliom_tools.menu" ]] [version;"Eliom_tools.html"]$</span>.
      First, define your menu like this:
      </p>
<pre><span class="Clet">let</span> mymenu current sp <span class="Cnonalphakeyword">=</span>
  <span class="Cconstructor">Eliom_tools</span>.menu <span class="Clabel">~classe:</span><span class="Cnonalphakeyword">[</span><span class="Cstring">"menuprincipal"</span><span class="Cnonalphakeyword">]</span>
    <span class="Cnonalphakeyword">(</span>home<span class="Cnonalphakeyword">,</span> &lt;:xmllist<span class="Cnonalphakeyword">&lt;</span> Home &gt;&gt;<span class="Cnonalphakeyword">)</span>
    <span class="Cnonalphakeyword">[</span>
     <span class="Cnonalphakeyword">(</span>infos<span class="Cnonalphakeyword">,</span> &lt;:xmllist<span class="Cnonalphakeyword">&lt;</span> More info &gt;&gt;<span class="Cnonalphakeyword">)</span><span class="Cnonalphakeyword">;</span>
     <span class="Cnonalphakeyword">(</span>tutorial<span class="Cnonalphakeyword">,</span> &lt;:xmllist<span class="Cnonalphakeyword">&lt;</span> Documentation &gt;&gt;<span class="Cnonalphakeyword">)</span>
   <span class="Cnonalphakeyword">]</span> current sp</pre>
      <p>Here, <code>home</code>,  <code>infos</code>,
        and <code>tutorial</code> are your three pages (generated for example
        by $a ~fragment:"VALnew_service" ~service:senddoc ~sp [code [pcdata "Eliom_services.new_service" ]] [version;"Eliom_services.html"]$).</p>



      <p>Then <code>mymenu ~service:home sp</code> will generate the following
        code:</p>
      <pre>&lt;ul class="menu menuprincipal"&gt;
  &lt;li class="current first"&gt;Home
  &lt;/li&gt;
  &lt;li&gt;&lt;a href="infos"&gt;More info&lt;/a&gt;
  &lt;/li&gt;
  &lt;li class="last"&gt;&lt;a href="tutorial"&gt;Documentation&lt;/a&gt;
  &lt;/li&gt;
&lt;/ul&gt;</pre>
    <p>Personalise it in your CSS style-sheet.</p>
    <p>$a ~fragment:"VALmenu" ~service:senddoc ~sp [code [pcdata "Eliom_tools.menu" ]] [version;"Eliom_tools.html"]$ takes a list of services without
    GET parameters.
    If you want one of the link to contains GET parameters, pre-apply
    the service.</p>
      <div class="encadre">
        <h4>How to make a menu entry with GET parameters?</h4>
          <p>
          Preapply your service.
          </p>
      </div>
      <h4>Hierarchical menus</h4>
      <p>

      </p>
*html*)
(* Hierarchical menu *)
open Eliom_tools

let hier1 = new_service ~path:["hier1"] ~get_params:unit ()
let hier2 = new_service ~path:["hier2"] ~get_params:unit ()
let hier3 = new_service ~path:["hier3"] ~get_params:unit ()
let hier4 = new_service ~path:["hier4"] ~get_params:unit ()
let hier5 = new_service ~path:["hier5"] ~get_params:unit ()
let hier6 = new_service ~path:["hier6"] ~get_params:unit ()
let hier7 = new_service ~path:["hier7"] ~get_params:unit ()
let hier8 = new_service ~path:["hier8"] ~get_params:unit ()
let hier9 = new_service ~path:["hier9"] ~get_params:unit ()
let hier10 = new_service ~path:["hier10"] ~get_params:unit ()

let mymenu =
  (
   (Main_page hier1),

   [([pcdata "page 1"], Site_tree (Main_page hier1, []));

    ([pcdata "page 2"], Site_tree (Main_page hier2, []));

    ([pcdata "submenu 4"],
     Site_tree
       (Default_page hier4,
         [([pcdata "submenu 3"],
          Site_tree
             (Not_clickable,
              [([pcdata "page 3"], Site_tree (Main_page hier3, []));
               ([pcdata "page 4"], Site_tree (Main_page hier4, []));
               ([pcdata "page 5"], Site_tree (Main_page hier5, []))]
             )
          );

          ([pcdata "page 6"], Site_tree (Main_page hier6, []))]
       )
    );

    ([pcdata "page 7"],
     Site_tree (Main_page hier7, []));

    ([pcdata "disabled"], Disabled);

    ([pcdata "submenu 8"],
     Site_tree
       (Main_page hier8,
        [([pcdata "page 9"], Site_tree (Main_page hier9, []));
         ([pcdata "page 10"], Site_tree (Main_page hier10, []))]
       )
    )
  ]
  )

let f i s sp () () =
  return
    (html
       (head (title (pcdata ""))
          ((style ~contenttype:"text/css"
             [cdata_style
 "a {color: red;}\n
  li.eliomtools_current > a {color: blue;}\n
  .breadthmenu li {\n
    display: inline;\n
    padding: 0px 1em;\n
    margin: 0px;\n
    border-right: solid 1px black;}\n
  .breadthmenu li.eliomtools_last {border: none;}\n
                "])::
                structure_links mymenu ~service:s ~sp)
             )
       (body [h1 [pcdata ("Page "^string_of_int i)];
              h2 [pcdata "Depth first, whole tree:"];
              div
                (hierarchical_menu_depth_first
                   ~whole_tree:true mymenu ~service:s ~sp);
              h2 [pcdata "Depth first, only current submenu:"];
              div (hierarchical_menu_depth_first mymenu ~service:s ~sp);
              h2 [pcdata "Breadth first:"];
              div
                (hierarchical_menu_breadth_first
                   ~classe:["breadthmenu"] mymenu ~service:s ~sp )]))


let _ =
  register hier1 (f 1 hier1);
  register hier2 (f 2 hier2);
  register hier3 (f 3 hier3);
  register hier4 (f 4 hier4);
  register hier5 (f 5 hier5);
  register hier6 (f 6 hier6);
  register hier7 (f 7 hier7);
  register hier8 (f 8 hier8);
  register hier9 (f 9 hier9);
  register hier10 (f 10 hier10)
(*html*
    </div>



    <h3 id="p3misc">Miscellaneous</h3>
    <div class="onecol">
    <h4>Several Ocaml modules for one site</h4>
      <p>If your site consists of several modules, you can load them
      consecutively from the configuration file using 
      <code>&lt;eliommodule&gt;</code> (same syntax as 
      <code>&lt;eliom&gt;</code>, the difference being that
      <code>&lt;eliommodule&gt;</code> does not generate any page).
      In that case, only the position of the
      <code>&lt;eliom&gt;</code>
      tag will be taken into account for generating the page using 
      Eliom.
      Note that there can be only one <code>&lt;eliom&gt;</code>
      tag for each <code>&lt;site&gt;</code>
      (or <code>&lt;host&gt;</code>).
      </p>
    <h4>Advanced use: create an extension for the server that access Eliom's data</h4>
      <p>If you want an Ocsigen extension with access to Eliom's
        data (for example if you want an extension that will
        register some services), you can use the function
        $a ~fragment:"VALregister_eliom_extension" ~service:senddoc ~sp [code [pcdata "Eliom_extensions.register_eliom_extension" ]] [version;"Eliom_extensions.html"]$
        to register the function that will generate the
        <code>Ocsigen_extensions.answer</code> from
        <code>sp</code>.
      </p>
    </div>



    <h3 id="p3examples">Examples</h3>
    <div class="onecol">
    <h4>Writing a forum</h4>
      <p>
      As an example,
      we will now write a small forum. Our forum has a main page,
      summarising all the messages and a page for each message.
      All the functions to access the database and print the result are
      left to the reader. We only want to show the structure of the site.
      Suppose you have written a function <code>news_headers_list_box</code>
      that writes the beginning of messages, and <code>message_box</code>
      that write a full message.
      </p>
*html*)(*zap* from ocsexample1 - attention la section Construction of pages a été simplifiée *zap*)(*html*
<pre>
<span class="Ccomment">(* All the services: *)</span>

<span class="Clet">let</span> main_page <span class="Cnonalphakeyword">=</span> new_service <span class="Clabel">~path:</span><span class="Cnonalphakeyword">[</span><span class="Cstring">""</span><span class="Cnonalphakeyword">]</span>
    <span class="Clabel">~get_params:</span>unit <span class="Cnonalphakeyword">(</span><span class="Cnonalphakeyword">)</span>

<span class="Clet">let</span> news_page <span class="Cnonalphakeyword">=</span> new_service <span class="Cnonalphakeyword">[</span><span class="Cstring">"msg"</span><span class="Cnonalphakeyword">]</span> <span class="Cnonalphakeyword">(</span>int <span class="Cstring">"num"</span><span class="Cnonalphakeyword">)</span> <span class="Cnonalphakeyword">(</span><span class="Cnonalphakeyword">)</span>

<span class="Ccomment">(* Construction of pages *)</span>

<span class="Clet">let</span> home sp () () <span class="Cnonalphakeyword">=</span>
  page sp
    <span class="Cnonalphakeyword">[</span>h1 [pcdata <span class="Cstring">"Mon site"</span>]<span class="Cnonalphakeyword">;</span>
     news_headers_list_box
       sp anonymoususer news_page<span class="Cnonalphakeyword">]</span>

<span class="Clet">let</span> print_news_page sp i () <span class="Cnonalphakeyword">=</span>
  page sp
    <span class="Cnonalphakeyword">[</span>h1 [pcdata <span class="Cstring">"Info"</span>]<span class="Cnonalphakeyword">;</span>
     message_box i anonymoususer<span class="Cnonalphakeyword">]</span>

<span class="Ccomment">(* Services registration *)</span>

<span class="Clet">let</span> <span class="Cnonalphakeyword">_</span> <span class="Cnonalphakeyword">=</span> register
  <span class="Clabel">~service:</span>main_page
  home

<span class="Clet">let</span> <span class="Cnonalphakeyword">_</span> <span class="Cnonalphakeyword">=</span> register
  <span class="Clabel">~service:</span>news_page
  print_news_page
</pre>



      <p>Now the same example with a login box on each page.
      We now have two versions of each page: connected and not connected.
      We need two actions (for connection and disconnection).
      Suppose we have the functions <code>login_box</code>,
      <code>connected_box</code>,
      and <code>connect</code>.
      </p>
*html*)(*zap* from ocsexample2 *zap*)(*html*
<pre><span class="Ccomment">(* All the services: *)</span>

<span class="Clet">let</span> main_page <span class="Cnonalphakeyword">=</span> new_service <span class="Clabel">~path:</span><span class="Cnonalphakeyword">[</span><span class="Cstring">""</span><span class="Cnonalphakeyword">]</span> <span class="Clabel">~get_params:</span>unit <span class="Cnonalphakeyword">(</span><span class="Cnonalphakeyword">)</span>

<span class="Clet">let</span> news_page <span class="Cnonalphakeyword">=</span> new_service <span class="Cnonalphakeyword">[</span><span class="Cstring">"msg"</span><span class="Cnonalphakeyword">]</span> <span class="Cnonalphakeyword">(</span>int <span class="Cstring">"num"</span><span class="Cnonalphakeyword">)</span> <span class="Cnonalphakeyword">(</span><span class="Cnonalphakeyword">)</span>

<span class="Clet">let</span> connect_action <span class="Cnonalphakeyword">=</span>
  new_post_coservice'
    <span class="Clabel">~post_params:</span><span class="Cnonalphakeyword">(</span>string <span class="Cstring">"login"</span> ** string <span class="Cstring">"password"</span><span class="Cnonalphakeyword">)</span>

<span class="Ccomment">(* Construction of pages *)</span>

let home sp () () =
   match get_volatile_session_data ~table:my_table ~sp () with
   | Eliom_sessions.Data_session_expired
   | Eliom_sessions.No_data -&gt;
     page sp
       [h1 [pcdata "My site"];
        login_box sp connect_action;
        news_headers_list_box sp anonymoususer news_page]
   | Eliom_sessions.Data user -&gt;
      page sp
        [h1 [pcdata "Mon site"];
         text_box "Bonjour !";
         connected_box sp user disconnect_action;
         news_headers_list_box sp user news_page]

let print_news_page sp i () =
   match get_volatile_session_data ~table:my_table ~sp () with
   | Eliom_sessions.Data_session_expired
   | Eliom_sessions.No_data -&gt;
      page sp
        [h1 [pcdata "Info"];
         login_box sp connect_action;
         message_box i anonymoususer]
   | Eliom_sessions.Data user -&gt;
      page sp
        [h1 [pcdata "Info"];
         connected_box sp user disconnect_action;
         message_box i user]

<span class="Ccomment">(* Services registration *)</span>

<span class="Clet">let</span> <span class="Cnonalphakeyword">_</span> <span class="Cnonalphakeyword">=</span> register
  <span class="Clabel">~service:</span>main_page
  home

<span class="Clet">let</span> <span class="Cnonalphakeyword">_</span> <span class="Cnonalphakeyword">=</span> register
  <span class="Clabel">~service:</span>news_page
  print_news_page

<span class="Clet">let</span> launch_session sp user <span class="Cnonalphakeyword">=</span>
  set_volatile_session_data my_table sp user

<span class="Clet">let</span> <span class="Cnonalphakeyword">_</span> <span class="Cnonalphakeyword">=</span> Eliom_predefmod.Actions.register
  <span class="Clabel">~action:</span>connect_action
    <span class="Cnonalphakeyword">(</span><span class="Cfun">fun</span> h <span class="Cnonalphakeyword">(</span>login<span class="Cnonalphakeyword">,</span> password<span class="Cnonalphakeyword">)</span> <span class="Cnonalphakeyword">-&gt;</span>
      launch_session sp <span class="Cnonalphakeyword">(</span>connect login password<span class="Cnonalphakeyword">)</span>; return []<span class="Cnonalphakeyword">)</span>
</pre>

    <h4>Miniwiki</h4>
    <p>Ocsigen's source code contains an example of Wiki written with
     Eliom by Janne Hellsten. It is called <em>Miniwiki</em>.
    </p>
    </div>

°°

*html*)
(*zap* *)


(* Main page for this example *)
let main = new_service [] unit ()

let _ = Eliom_predefmod.Xhtmlcompact.register main
  (fun sp () () ->
    (* Do not register a page after initialisation.
       This will cause an error:
       let coucou6 =
       new_service
        ~path:["coucou6"]
        ~server_params:no_server_param
        ~get_params:no_get_param
        ()
       in *)
    (* This will be ignored: register coucou1 << <html></html> >>; *)
    return
     <<
       <html>
       <!-- This is a comment! -->
       <head>
         $css_link (make_uri ~service:(static_dir sp) ~sp ["style.css"]) ()$
         <title>Eliom Tutorial</title>
       </head>
       <body>

         <h1>$img ~alt:"Ocsigen" ~src:(Eliom_predefmod.Xhtml.make_uri ~service:(static_dir sp) ~sp ["ocsigen5.png"]) ()$</h1>

       <h3>Eliom examples</h3>
       <h4>Simple pages</h4>
       <p>
         A simple page: $a coucou sp <:xmllist< <code>coucou</code> >> ()$ <br/>
         A page with a counter: $a count sp <:xmllist< <code>count</code> >> ()$ <br/>
         A page in a directory:
           $a hello sp <:xmllist< <code>dir/hello</code> >> ()$ <br/>
       Default page of a directory:
           $a default sp <:xmllist< <code>rep/</code> >> ()$</p>
       <h4>Parameters</h4>
       <p>
         A page with GET parameters:
           $a coucou_params sp <:xmllist< <code>coucou</code> with params >> (45,(22,"krokodile"))$ (what if the first parameter is not an integer?)<br/>
         A page with "suffix" URL that knows the IP and user-agent of the client:
           $a uasuffix sp <:xmllist< <code>uasuffix</code> >> (2007,6)$ <br/>
         A page with "suffix" URL and GET parameters :
           $a isuffix sp <:xmllist< <code>isuffix</code> >> ((111, ["OO";"II";"OO"]), 333)$ <br/>
         A page with a parameter of user-defined type :
             $a mytype sp <:xmllist< <code>mytype</code> >> A$ </p>
       <h4>Links and Formulars</h4>
       <p>
         A page with links: $a links sp <:xmllist< <code>links</code> >>  ()$ <br/>
         A page with a link towards itself:
             $a linkrec sp <:xmllist< <code>linkrec</code> >> ()$ <br/>
         The $a main sp <:xmllist< default page >> ()$
             of this directory (myself) <br/>
         A page with a GET form that leads to the "coucou" page with parameters:
             $a form sp <:xmllist< <code>form</code> >> ()$ <br/>
         A POST form towards the "post" page:
             $a form2 sp <:xmllist< <code>form2</code> >> ()$ <br/>
         The "post" page, when it does not receive parameters:
             $a no_post_param_service sp <:xmllist< <code>post</code> without post_params >> ()$ <br/>
         A POST form towards a service with GET parameters:
             $a form3 sp <:xmllist< <code>form3</code> >> ()$ <br/>
         A POST form towards an external page:
             $a form4 sp <:xmllist< <code>form4</code> >> ()$ </p>
       <h4>Sessions</h4>
       <p>
         Coservices:
             $a coservices_example sp <:xmllist< <code>coservice</code> >> ()$ <br/>
         A session based on cookies, implemented with session data:
             $a session_data_example sp <:xmllist< <code>sessdata</code> >> ()$ <br/>
         A session based on cookies, implemented with actions:
             $a connect_example3 sp <:xmllist< <code>actions</code> >> ()$ <br/>
         A session based on cookies, with session services:
             $a session_services_example sp <:xmllist< <code>sessionservices</code> >> ()$ <br/>
         A session based on cookies, implemented with actions, with session groups:
             $a connect_example5 sp <:xmllist< <code>groups</code> >> ()$ <br/>
         The same with wrong user if not "toto":
             $a connect_example6 sp <:xmllist< <code>actions2</code> >> ()$ <br/>
         Coservices in the session table:
             $a calc sp <:xmllist< <code>calc</code> >> ()$ <br/>
       <!--  (ancienne version : $a shop_without_post_params sp <:xmllist< <code>shop</code> >> ()$) -->
         Persistent sessions:
             $a persist_session_example sp <:xmllist< <code>persist</code> >> ()$ <br/>
       </p>
       <h4>Other</h4>
       <p>
       A page that is very slow, implemented in cooperative way:
             $a looong sp <:xmllist< <code>looong</code> >> ()$<br/>
       A page that is very slow, using preemptive threads:
             $a looong sp <:xmllist< <code>looong2</code> >> ()$<br/>
       Catching errors:
             $a catch sp <:xmllist< <code>catch</code> >> 22$ (change the value in the URL)<br/>
       Redirection:
             $a redir sp <:xmllist< <code>redir</code> >> 11$<br/>
       Cookies:
             $a cookies sp <:xmllist< <code>cookies</code> >> ()$<br/>
       Disposable coservices:
             $a disposable sp <:xmllist< <code>disposable</code> >> ()$<br/>
       Coservice with timeout:
             $a timeout sp <:xmllist< <code>timeout</code> >> ()$<br/>
       Public coservice created after initialization (with timeout):
             $a publiccoduringsess sp <:xmllist< <code>publiccoduringsess</code> >> ()$<br/>
       The following URL send either a statically checked page, or a text page:
             $a send_any sp <:xmllist< <code>send_any</code> >> "valid"$<br/>
       A page with a persistent counter:
             $a count2 sp <:xmllist< <code>count2</code> >> ()$ <br/>
       $a hier1 sp [pcdata "Hierarchical menu"] ()$ <br/>
       $a divpage sp <:xmllist< <code>a link sending a &lt;div&gt; page</code> >> ()$ <br/>
       </p>
       <h4>Advanced forms</h4>
       <p>
       A page that parses a parameter using a regular expression:
          $a regexpserv sp <:xmllist< <code>regexpserv</code> >> "[toto]"$.<br/>
       A page that takes a set of parameters:
             $a set sp <:xmllist< <code>set</code> >> ["Ciao";"bello";"ciao"]$ <br/>
       A form to the previous one:
             $a setform sp <:xmllist< <code>setform</code> >> ()$ <br/>
       A page that takes any parameter:
             $a raw_serv sp <:xmllist< <code>raw_serv</code> >> [("a","hello"); ("b","ciao")]$ <br/>
       A form to the previous one:
             $a raw_form sp <:xmllist< <code>raw_form</code> >> ()$ <br/>
       A form for a list of parameters:
             $a listform sp <:xmllist< Try it >> ()$.<br/>
       </p>
       </body>
     </html> >>)


(* *zap*)
