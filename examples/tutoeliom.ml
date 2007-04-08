(*zap* 
   this is the Eliom Tutorial.
   It is mainly written in html.
   You can find a more readable version of comments on http://www.ocsigen.org
*zap*)
(*zap*
~/bin/caml2html -css -hc2 -oc tutoeliom.ml
*zap*)
(*html*
    <div class="twocol1">
      <p>This is the tutorial for <em>Eliom</em> (development version).
        Eliom is the new module for page generation
        for the <em>Ocsigen</em> Web server.
       (Please report any error in this tutorial).</p>
      <p>Eliom is an extension for the Web server <em>Ocsigen</em>
         that allows dynamic generation of pages.
         It uses very new concepts making programming very different
         from all other Web programming tools.
         It allows to write a complex Web site in very few lines of code.
      </p>
    </div>
    <div class="twocol2">
      <p>
        The old <em>Ocsigenmod</em> used in version 0.6.0 is now deprecated
        but you can still use it.
        <em>Eliom</em> is very close to <em>Ocsigenmod</em>. Switching
        to <em>Eliom</em> should be easy.
        Have a look a $a toeliom sp [pcdata "that page"] ()$ 
        to learn quickly how to adapt your sites.
      </p>
      <p><em>Warning: This tutorial assumes you know the 
        <em>Objective Caml</em> language.</em></p>
    </div>
    <h2>Base principles</h2>
    <div class="twocol1">
      <p>With Eliom, you don't write one file for each URL.
          You write a caml module (cmo or cma) for your whole website.</p>
      <p>
          The <code>Eliom</code> module allows to create new entry points to 
          your Web site, called <em>services</em>. On each of these services,
          you must register a function that will generate a page.
          There are several ways to creates pages for Eliom. This tutorial
          is mainly using <code>Eliom.Xhtml</code>, a module allowing
          to register xhtml pages statically typed using OCaml's
          polymorphic variants. 
          The <code>XHTML.M</code> module defines functions to construct
          xhtml pages using that type system. 
          As the <code>Eliom.Xhtml</code> redefines some functions
          of <code>XHTML.M</code>, open them in this order:
      </p>
*html*)
open XHTML.M
open Eliom
open Eliom.Xhtml
open Lwt
(*html*
      <p><code>Lwt</code> is the cooperative thread library used by Ocsigen
      (see later).</p>
      <p>Here is an example showing how to create a new service and
         register a page created with XHTML.M. Use the function
         <code>Eliom.Xhtml.register_new_service</code>:
      </p>
*html*)
let coucou = 
  register_new_service 
    ~url:["coucou"]
    ~get_params:unit
    (fun _ () () -> 
      return 
        (html
          (head (title (pcdata "")) [])
          (body [h1 [pcdata "Hallo"]])))
(*html*
      <p><code>return</code> is a function from <code>Lwt</code>.
      Use it as this for now, and see later for more advanced use.</p>
      <p>
      Now you can compile your file (here tutorial.ml) by doing:</p>
      <pre>ocamlc -I /<em>path_to</em>/ocsigen/ -c tutorial.ml</pre>
      <p>
      (Replace <code>/<em>path_to</em>/ocsigen/</code>
       by the directory where ocsigen is installed).
      </p>
      <p>
      Add the following lines to Ocsigen's config file 
      (usually <code>/etc/ocsigen/ocsigen.conf</code>):
      </p>
      <pre>&lt;host&gt;
 &lt;site dir="examples"&gt;
  &lt;module file="/<em>path_to</em>/tutoeliom.cmo" /&gt;
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
   $a (static_dir sp) sp [pcdata "Makefile"] "Makefile"$ for your modules.</p>
      <h3>Static typing of XHTML</h3>
        <p>
        Typing of xhtml with XHTML.M and Eliom.Xhtml
        is very strict and forces you to respect
        xhtml 1.1 standard (with some limitations). 
        For example if you write:
        </p>
<pre><span class="Cnonalphakeyword">(</span>html
   <span class="Cnonalphakeyword">(</span>head <span class="Cnonalphakeyword">(</span>title <span class="Cnonalphakeyword">(</span>pcdata <span class="Cstring">""</span><span class="Cnonalphakeyword">)</span><span class="Cnonalphakeyword">)</span> <span class="Cnonalphakeyword">[</span><span class="Cnonalphakeyword">]</span><span class="Cnonalphakeyword">)</span>
   <span class="Cnonalphakeyword">(</span>body <span class="Cnonalphakeyword">[</span>pcdata <span class="Cstring">"Hallo"</span><span class="Cnonalphakeyword">]</span><span class="Cnonalphakeyword">)</span><span class="Cnonalphakeyword">)</span></pre>
    </div>
    <div class="twocol2">
        <p>You have the following error message:</p>
<pre>This expression has type ([&gt; `PCDATA ] as 'a) XHTML.M.elt
but is here used with type 
([&lt; XHTML.M.block ] as 'b) XHTML.M.elt
Type 'a is not compatible with type
'b =
  [&lt; `Address | `Blockquote | `Del | `Div | `Dl | `Fieldset
   | `Form | `H1 | `H2 | `H3 | `H4 | `H5 | `H6 | `Hr | `Ins
   | `Noscript | `Ol | `P | `Pre | `Script | `Table | `Ul ]</pre>
   <p><code>'b</code> is the list of tags allowed in a 
        block tag (here <code>&lt;body&gt;</code>), but PCDATA
        (i.e. raw text) is not allowed here.</p>
   <p>In XHTML, some tags cannot be empty. For example 
   <code>&lt;table&gt;</code> must contains at least one row.
   To enforce this, the <code>table</code> function takes two parameters:
   the first one is the first row, the second one is a list
   containig all the other rows.
   (same thing for <code>&lt;tr&gt;</code> <code>&lt;form&gt;</code>
<code>&lt;dl&gt;</code> <code>&lt;ol&gt;</code> <code>&lt;ul&gt;</code>
<code>&lt;dd&gt;</code> <code>&lt;select&gt;</code> ...)
   </p>
      <div class="encadre">
        <h3>Alternate syntax</h3>
          <p>
          If you prefer using a syntax closer to html, you can write:</p>
*html*)
let coucou1 = 
  register_new_service 
    ~url:["coucou1"]
    ~get_params:unit
    (fun _ () () -> return
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
         You can mix the two syntaxes (see later).
      </p>
      <p>
         <em>Warning:</em> The two syntaxes are not equivalent for typing.
         Using the syntax extension will do less verifications. 
         For example the following code is accepted but not valid
         with respect to the xhtml's dtd (because <code>&lt;head&gt;</code>
         must contain a title):
      </p>
<pre>&lt;&lt; <span class="Cnonalphakeyword">&lt;</span>html<span class="Cnonalphakeyword">&gt;</span>
     <span class="Cnonalphakeyword">&lt;</span>head&gt;&lt;/head<span class="Cnonalphakeyword">&gt;</span>
     <span class="Cnonalphakeyword">&lt;</span>body&gt;&lt;h1<span class="Cnonalphakeyword">&gt;</span>plop&lt;/h1&gt;&lt;/body<span class="Cnonalphakeyword">&gt;</span>
   &lt;/html<span class="Cnonalphakeyword">&gt;</span> &gt;&gt;</pre>
      <p>
        We recommand to use preferably 
        the functions from <code>XHTML.M</code>, as you will (almost)
        always get valid xhtml.
        Use the syntax extension for example to enclose already created pieces
        of html, and verify the validity of your pages with the
        $a (new_external_service ["http://validator.w3.org"] unit unit ()) 
           sp <:xmllist< W3C validator >> ()$.
      </p>
      <p>
        $a (new_external_service
              ["doc/0.7.0/XHTML.M.html"]
(*              ["http://theorie.physik.uni-wuerzburg.de/~ohl/xhtml/"] *)
              unit unit ())
           sp <:xmllist< More info >> ()$
        on <code>XHTML.M</code>.
      </p>
      <p>
       $a xhtmlsyntax sp <:xmllist< More info >> ()$ on the syntax extension.
      </p>
      </div>
      <div class="encadre">
        <h3>Eliom and OCamlDuce</h3>
        <p>If OCamlDuce is installed on your system, it is now possible to use
        it instead of XHTML.M and Eliom.Xhtml
        to typecheck your pages. You get a stronger type checking
        and more flexibility (easier to use other XML types, easier to parse
        incoming XML data, etc.).</p>
        <p>To use it, make sure that you have Eliom compiled with OCamlDuce 
         support. Then dynlink <code>ocamlduce.cma</code> and 
          <code>eliomduce.cma</code> from the configuration file
        (after <code>eliom.cma</code>).
        Then use <code>Eliomduce.Xhtml</code> instead of 
        <code>Eliom.Xhtml</code> to register your pages.
        </p>
        <p>Here is an example:</p>
        <pre><span style="color:#cc9900">open</span> <span style="color:#0033cc">Eliom</span>
<span style="color:#cc9900">open</span> <span style="color:#0033cc">Eliomduce</span>.<span style="color:#0033cc">Xhtml</span>
<span style="color:#cc9900">open</span> <span style="color:#0033cc">Lwt</span>

<span style="color:green">let</span> s =
  register_new_service
    <span style="color:#770000">~url:</span>[<span style="color:#aa4444">""</span>]
    <span style="color:#770000">~get_params:</span>unit
    (<span style="color:green">fun</span> sp () () -&gt;
      return
        {{ &lt;html&gt;
             [&lt;head&gt; [&lt;title&gt; <span style="color:#aa4444">""</span>]
              &lt;body&gt; [&lt;h1&gt; <span style="color:#aa4444">"This page has been type checked by OcamlDuce"</span>]] }}) </pre>
      </div>
      <div class="encadre">
        <h3>Eliom.Text</h3>
        <p>If you want to register untyped (text) pages, use the
         functions from <code>Eliom.Text</code>, for example
         <code>Eliom.Text.register_new_service</code>.
        </p>
      </div>
    </div>
    <h2>More examples</h2>
    <div class="twocol1">
      <p>Services registered with <code>register_new_service</code>
         are available for all users. We call them <em>public services</em>.
      </p>
      <p>
        Page generation may have side-effects:
      </p>
*html*)
let compt = 
  let next =
    let c = ref 0 in
      (fun () -> c := !c + 1; !c)
  in
  register_new_service 
    ~url:["compt"]
    ~get_params:unit
    (fun _ () () ->  return
      (html
       (head (title (pcdata "counter")) [])
       (body [p [pcdata (string_of_int (next ()))]])))
(*html*
      <p>
      See this example $a Tutoeliom.compt sp <:xmllist< here >> ()$.
      </p>
      <p>As usual in OCaml, you can forget labels when the application 
          is total:</p>
*html*)
let hello = 
  register_new_service 
    ["dir";"hello"]  (* the url dir/hello *)
    unit
    (fun _ () () ->  return
      (html
         (head (title (pcdata "Hello")) [])
         (body [h1 [pcdata "Hello"]])))
(*html*
      <p>
      See this example $a Tutoeliom.hello sp <:xmllist< here >> ()$.
      </p>
    </div>
    <div class="twocol2">
      <p>The last example shows how to define the default page for
       a directory. (Note that <code>["rep";""]</code> is equivalent to 
        <code>["rep";"index"]</code>.)</p>
*html*)
let default = register_new_service ["rep";""] unit
  (fun _ () () -> return
    (html
      (head (title (pcdata "")) [])
      (body [p [pcdata "default page. rep is redirected to rep/"]])))
(*html*
      <p>
      See $a Tutoeliom.default sp <:xmllist< default >> ()$.
      </p>
    </div>
    <h2>Parameters</h2>
    <div class="twocol1">
      <p>The parameter labelled 
        <code><span class="Clabel">~get_params</span></code>
        indicates the type of GET parameters for the page (that is, parameters
        present in the URL).
        <code>unit</code> means that the page does not take any GET parameter.
      </p>
      <p>Functions implementing services take three parameters. The first
       one has type <code>Eliom.server_params</code> and
       corresponds to server informations (user-agent, ip, current-url, etc.
       - see later), the second one is for GET parameters 
        (that is, parameters in the URL) and the third one
       for POST parameters (parameters in the body of the HTTP request).</p>
      <p>Here is an example of a service with GET parameters:</p>
*html*)
let writeparams _ (i1, (i2, s1)) () =  return
  (html
    (head (title (pcdata "")) [])
    (body [p [pcdata "You sent: ";
              strong [pcdata (string_of_int i1)];
              pcdata ", ";
              strong [pcdata (string_of_int i2)];
              pcdata " and ";
              strong [pcdata s1]]]))
(*zap* you can register twice the same service, with different parameters names 
 *zap*)
let coucou_params = register_new_service 
    ~url:["coucou"]
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
      If <code>entier</code> is not an integer,
      the server displays an error-message.<br/>
      <em>Warning:</em>
      The infix function <code>( ** )</code> is to be used to 
      construct <em>pairs</em> (not tuples).
      </p>
      <p>The following examples shows how to create a service with "suffix" 
         service
         (taking the end of the URL as a parameter, as wikis do very often)
        and how to get values from the http header:</p>
*html*)
let uasuffix = 
  register_new_service 
    ~url:["uasuffix";""]
    ~get_params:(suffix (string "s"))
    (fun sp (suff, s) () ->  return
      (html
        (head (title (pcdata "")) [])
        (body
           [p [pcdata "The suffix of the url is ";
               strong [pcdata (string_of_url_path suff)];
               pcdata ", your user-agent is ";
               strong [pcdata (get_user_agent sp)];
               pcdata ", your IP is ";
               strong [pcdata (get_ip sp)];
               pcdata " and s is ";
               strong [pcdata s]]])))
(*html*
    </div>
    <div class="twocol2">
*html*)
let isuffix = 
  register_new_service 
    ~url:["isuffix";""] 
    ~get_params:(suffix (int "i"))
    (fun sp (suff, i) () -> return
      (html
        (head (title (pcdata "")) [])
        (body
           [p [pcdata "The suffix of the url is ";
               strong [pcdata (string_of_url_path suff)];
               pcdata " and i is equal to ";
               strong [pcdata (string_of_int i)]]])))
(*html*
      <p>See $a Tutoeliom.uasuffix sp <:xmllist< uasuffix >> ("suffix", "gni")$,
         $a Tutoeliom.isuffix sp <:xmllist< isuffix >> ("mm/aa/gg", 22)$.</p>

      <p>The following example shows how to use your own types:</p>
*html*)
type mysum = A | B
let mysum_of_string = function
    "A" -> A
  | "B" -> B
  | _ -> raise (Failure "mysum_of_string")
let string_of_mysum = function
    A -> "A"
  | B -> "B"

let mytype = register_new_service 
  ["mytype"]
  (user_type mysum_of_string string_of_mysum "valeur")
  (fun _ x () -> let v = string_of_mysum x in  return
    (html
       (head (title (pcdata "")) [])
       (body [p [pcdata v]])))
(*html*
      <p>See $a Tutoeliom.mytype sp <:xmllist< mytype >> Tutoeliom.A$.</p>
      <div class="encadre">
        <h3>Catching errors</h3>
        <p>You can catch typing errors of parameters
        using the optional parameter
        <code>error_handler</code>:</p>
*html*)
let catch = register_new_service
    ~url:["catch"]
    ~get_params:(int "i")
    ~error_handler:(fun sp l ->  return
      (html
         (head (title (pcdata "")) [])
         (body [p [pcdata ("i is not an integer.")]])))
    (fun _ i () -> let v = string_of_int i in  return
    (html
       (head (title (pcdata "")) [])
       (body [p [pcdata ("i is an integer: "^v)]])))
(*html*
      <p><code>error_handler</code> takes as parameters the usual 
         <code>sp</code>, and a list of pairs <code>(n,ex)</code>,
         where <code>n</code> is the name of the wrong parameter, and
         <code>ex</code> is the exception that has been raised while
         parsing its value.</p>
      <!-- p>See $a Tutoeliom.catch sp <:xmllist< catch >> 22$ (change the value
   of the parameter).</p -->
     </div>
    </div>
    <h2>Links</h2>
    <div class="twocol1">
      <p>To create a link (anchor), use the function 
          <code>Eliom.Xhtml.a</code>
      </p>
*html*)
let links = register_new_service ["rep";"links"] unit
 (fun sp () () -> return
   (html
     (head (title (pcdata "")) [])
     (body 
       [p
        [a coucou sp [pcdata "coucou"] (); br ();
         a hello sp [pcdata "hello"] (); br ();
         a default sp 
           [pcdata "default page of the dir"] (); br ();
         a uasuffix sp 
           [pcdata "uasuffix"] (["suff";"ix"],"toto"); br ();
         a coucou_params sp 
           [pcdata "coucou_params"] (42,(22,"ciao")); br ();
         a
           (new_external_service
              ~url:["http://fr.wikipedia.org";"wiki";""]
              ~get_params:(suffix_only ())
              ~post_params:unit ()) 
           sp
           [pcdata "ocaml on wikipedia"]
           ["OCaml"]]])))
(*zap* 
   Note that to create a link we need to know the current url, because:
   the link from toto/titi to toto/tata is "tata" and not "toto/tata"
*zap*)
(*html*
      <p>See $a Tutoeliom.links sp <:xmllist< links >> ()$.</p>
      <p>
      Note that to create a (relative) link we need to know the current URL.
      That's why the page has a <code>sp</code> parameter.<br/>
      The link to Wikipedia shows how to define an external service (here it 
      uses a suffix URL).</p>
      <p>
      The last parameter of <code>Eliom.Xhtml.a</code> is for
      GET parameters you want to put in the link.
      The type of this parameter and the name of GET parameters depend
      on the service you link to.
      </p>
    </div>
    <div class="twocol2">
      <p>
        If you want to create (mutually or not) recursive pages,
        first create the service using <code>Eliom.new_service</code>, 
        then register it in the table using (for example)
        <code>Eliom.Xhtml.register</code>:
      </p>
*html*)
let linkrec = new_service ["linkrec"] unit ()

let _ = register linkrec 
    (fun sp () () ->  return
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
(*html*
      <p>See $a Tutoeliom.linkrec sp <:xmllist< linkrec >> ()$.</p>
      <p>The server won't accept to start if there are
         unregistered services.</p>
    </div>
    <h2>Forms</h2>
    <div class="twocol1">
      <p>The function <code>Eliom.get_form</code> allows to create a form
      that uses the GET method (parameters in the URL).
      It works like <code>Eliom.Xhtml.a</code> but takes as parameter
      a <em>function</em> that creates the form from parameters names.
      </p>
*html*)
let create_form = 
  (fun (number_name,(number2_name,string_name)) ->
    [p [pcdata "Write an int: ";
        int_input number_name;
        pcdata "Write another int: ";
        int_input number2_name;
        pcdata "Write a string: ";
        string_input string_name;
        submit_input "Click"]])
(*zap*        
    <:xmllist< <p>Write an int: $int_input entier$ <br/>
    Write a string: $password_input chaine$ <br/>
    Write a string: $string_input chaine2$ <br/>
    $submit_input "Click"$</p>
    >>)
*zap*)
let form = register_new_service ["form"] unit
  (fun sp () () -> 
     let f = get_form coucou_params sp create_form in 
     return
       (html
         (head (title (pcdata "")) [])
         (body [f])))
(*html*
      <p>See the function $a Tutoeliom.form sp <:xmllist< form >> ()$ in action.</p>

      <h3>POST parameters</h3>
      <p>
   By default parameters of a web page are in the URL (GET parameters).
   A web page may expect parameters from the http header (POST parameters,
   that is, parameters which are not in the URL but in the body of the HTTP
   request).
   Use this if you don't want the user to be able to bookmark
   the URL with parameters, for example if you want to post some
   data that will change the state of the server (database, paiement, etc).
   When designing a Web site, think carefully about the choice between
   GET or POST method for each service!
   </p>
   <p>
   When you register a service with POST parameters, you must register
   before a service (fallback) without these parameters (for example that will
   answer if the page is reloaded without the hidden parameters, or
   if it is bookmarked).
      </p>
*html*)
let no_post_param_service = 
  register_new_service 
    ~url:["post"]
    ~get_params:unit
    (fun _ () () -> return
      (html
         (head (title (pcdata "")) [])
         (body [h1 [pcdata 
                      "Version of the page without POST parameters"]])))
    
let my_service_with_post_params = 
  register_new_post_service
    ~fallback:no_post_param_service
    ~post_params:(string "value")
    (fun _ () value ->  return
      (html
         (head (title (pcdata "")) [])
         (body [h1 [pcdata value]])))
(*html*
      <p>Services may take both GET and POST parameters:</p>
*html*)
let get_no_post_param_service = 
  register_new_service 
    ~url:["post2"]
    ~get_params:(int "i")
    (fun _ i () -> 
      return
        (html
         (head (title (pcdata "")) [])
         (body [p [pcdata "No POST parameter, i:";
                   em [pcdata (string_of_int i)]]])))
(*html*
    </div>
    <div class="twocol2">
*html*)
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
       <p> To create a POST form, use the <code>post_form</code> function,
           possibly applied to GET parameters (if any).
           Here <code>form2</code> is a page containing a form
           to the service <code>post</code> (using XHTML.M's functions) 
           and <code>form3</code> (defined using the syntax extension)
           contains a form to <code>post2</code>, with a GET parameter.
           <code>form4</code> is a form to an external page.
       </p>
*html*)
let form2 = register_new_service ["form2"] unit
  (fun sp () () -> 
     let f =
       (post_form my_service_with_post_params sp
          (fun chaine -> 
            [p [pcdata "Write a string: ";
                string_input chaine]]) ()) in
     return
       (html
         (head (title (pcdata "form")) [])
         (body [f])))

let form3 = register_new_service ["form3"] unit
  (fun sp () () ->
     let f  = 
       (post_form my_service_with_get_and_post sp
          (fun chaine -> 
            <:xmllist< <p> Write a string: 
                    $string_input chaine$ </p> >>)
          222) in  return
       << <html>
            <head><title></title></head>
            <body>$f$</body></html> >>)

let form4 = register_new_service ["form4"] unit
  (fun sp () () ->
     let f  = 
       (post_form
          (new_external_service 
             ~url:["http://www.petizomverts.com"]
             ~get_params:(int "i")
             ~post_params:(string "chaine") ()) sp
          (fun chaine -> 
            <:xmllist< <p> Write a string: 
	             $string_input chaine$ </p> >>)
          222) in return
     (html
        (head (title (pcdata "form")) [])
        (body [f])))
(*html*
      <p>See the url
      $a Tutoeliom.no_post_param_service sp <:xmllist< <code>post</code> without parameter >> ()$,
      $a Tutoeliom.get_no_post_param_service sp <:xmllist< <code>post2</code> without POST parameter >> 123$,
      $a Tutoeliom.form2 sp <:xmllist< form2 >> ()$,
      $a Tutoeliom.form3 sp <:xmllist< form3 >> ()$,
      $a Tutoeliom.form4 sp <:xmllist< form4 >> ()$.
      </p>

    </div>
    <h2>Summary of concepts</h2>
    <div class="encadre sanstitre">
      <p>Here is a summary of the concepts that will be developped in
         the following of this tutorial.</p>
      <p>Eliom uses three kinds of services:</p>
      <dl>
        <dt>Main services</dt><dd>are the main entry points of your sites.
        Created by <code>new_service</code> or 
        <code>new_post_service</code>.</dd>
        <dt>Coservices</dt><dd>are services often created dynamically for
        one user (often in the session table) or used to particularize one
        button but not the page it leads to (like the disconnect button
        in the example of sessions with actions below).
        </dd>
        <dt>Non-attached coservices</dt><dd>are coservices towards the current
        URL.
       </dd>
      </dl>
      <p>Each of these services has its a version with POST parameters.
      Remember to use POST parameters when you want a different behaviour
      between the first click and a reload of the page. Usually sending
      POST parameters triggers an action on server side, and you don't want 
      it to succeed several times.</p>
   
    </div>
    <div class="encadre sanstitre">
      <p>You can register several kinds of pages on these services,
      using these different modules:</p>
      <dl>
        <dt>Eliom.Xhtml</dt><dd>allows to register functions that 
        generate xhtml pages
        checked statically using polymorphic variant types. You may use
        constructor functions from <code>XHTML.M</code> or a syntax
        extension close to the standard xhtml syntax.</dd>
        <dt>Eliomduce</dt><dd>allows to register functions that generate
        xhtml pages 
        checked statically using <code>Ocamlduce</code>. Typing is more
        strict, but you need a modified version of the OCaml compiler 
        (Ocamlduce).</dd>
        <dt>Eliom.Text</dt><dd>Allows to register functions that
        generate text pages, without any typechecking of the content.
        </dd>
        <dt>Eliom.Actions</dt><dd>allows to register actions, that is
        functions that do not generate any page. The URL is reloaded after
        the action.
        </dd>
        <dt>Eliom.Unit</dt><dd>is like <code>Eliom.Actions</code> but the
        URL is not reloaded after the action.</dd>
        <dt>Eliom.Redirections</dt><dd>allows to register redirections
        </dd>
        <dt>Eliom.Files</dt><dd>allows to register services that send files
        </dd>
        <dt>Eliom.Any</dt><dd>allows to register services that can choose
            what they send
        </dd>
      </dl>
   
    </div>
    <div class="encadre sanstitre">
      <p>Each of these registrations may be done in the <em>public</em>
      table, or in a <em>session</em> table, accessible only for one
      user of the Web site.
      </p>

      <p>Eliom will try to find the page, in that order:</p>
      <ul>
       <li>in the session table,</li>
       <li>in the public table,</li>
       <li>the fallback in the session table, if the coservice has expired,</li>
       <li>the fallback in the public table, if the session has expired.</li>
      </ul>

      <p>Details on service registration:</p>
      <ul>
        <li>All services created during initialisation must be registered
        in the public table
        during the initialisation phase of your module.
        If not, the server will not start (with an error message in the logs).
        This is to ensure that all the URLs the user can bookmark
        will always give an answer, even if the session has expired.</li>
        <li>Services cannot be registered in the public table
         after the initialisation phase.
        </li>
        <li>Do not register twice the same service in the public table, 
          and do not replace a service
          by a directory (or vice versa). If this happens during the 
          initialisation phase, the server won't start.
          If this happens after, it will be ignored (with a warning in the 
          logs).
        </li>
        <li>All services (but non-attached ones) must be created in
        a module loaded inside a <code>&lt;site&gt;</code> tag of the
        config file (because they will be attached to a directory).
        </li>
        <li>GET coservices (whithout POST parameters) can be registered
        only with a main service without GET/POST parameters as fallback.
        But it may be a preapplied service.
        </li>
        <li>Services with POST parameters (main service or coservice) 
        can be registered with a (main or co) service without POST
        parameters as fallback.</li>
      </ul>



    </div>
    <h2>Sessions</h2>
    <div class="twocol1">
      <p>
      Eliom allows to replace a public service by a service valid only for
      one user.
      To create a "session service", register the service in
      a "session table" (valid only for one client) 
      instead of the public table. To do that,
      use <span class="Cem"><code>register_for_session</code></span>.
      </p><p>
      Users are recognized automatically using a cookie.
      Use this for example if you want two versions of each page,
      one public, one for connected users.
      <br/>        
      To close a session, use 
                <span class="Cem"><code>close_session</code></span>.</p>
      <p>Note that <code>register_for_session</code>
         and <code>close_session</code> take <code>sp</code> as parameter
         (because sp contains the session table).</p>
      <p>The following is an example of web site that behaves differently
      when users are connected.
      We first define the main page, with a login form:
      </p>
*html*)
let public_session_without_post_params = 
  new_service ~url:["session"] ~get_params:unit ()

let public_session_with_post_params = 
  new_post_service 
    ~fallback:public_session_without_post_params
    ~post_params:(string "login")
    ()

let accueil sp () () = 
  let f = post_form public_session_with_post_params sp
    (fun login -> 
         [p [pcdata "login: ";
             string_input login]]) () in return
  (html
     (head (title (pcdata "")) [])
     (body [f]))


let _ = register
  ~service:public_session_without_post_params
  accueil

let close = register_new_service
    ~url:["disconnect"]
    ~get_params:unit
    (fun sp () () -> close_session sp; 
      return
        (html
           (head (title (pcdata "Disconnect")) [])
           (body [p [pcdata "You have been disconnected. ";
                     a public_session_without_post_params
                       sp [pcdata "Retry"] ()
                   ]])))


(*html*
    <p>When the page is called with login parameters,
       it runs the function <code>launch_session</code>
       that replaces some services already defined by new ones:
    </p>
    </div>
    <div class="twocol2">
*html*)
let launch_session sp () login =
  let new_main_page sp () () = return
    (html
       (head (title (pcdata "")) [])
       (body [p [pcdata "Welcome ";
                 pcdata login; 
                 pcdata "!"; br ();
                 a coucou sp [pcdata "coucou"] (); br ();
                 a hello sp [pcdata "hello"] (); br ();
                 a links sp [pcdata "links"] (); br ();
                 a close sp [pcdata "close session"] ()]]))
  in
  register_for_session 
    sp
    ~service:public_session_without_post_params 
    (* service is any public service already registered *)
    new_main_page;
  register_for_session 
    sp
    ~service:coucou
    (fun _ () () -> return
      (html
         (head (title (pcdata "")) [])
         (body [p [pcdata "Coucou ";
                   pcdata login;
                   pcdata "!"]])));
  register_for_session 
    sp
    hello
    (fun _ () () -> return
      (html
         (head (title (pcdata "")) [])
         (body [p [pcdata "Ciao ";
                   pcdata login;
                   pcdata "!"]])));
  new_main_page sp () ()
    
let _ =
  register
    ~service:public_session_with_post_params
    launch_session
(*zap* Registering for session during initialisation is forbidden:
let _ = register_for_session
    ~url:coucou1 
    << <html>
         <head><title></title></head>
         <body><h1>humhum</h1></body>
       </html> >>
*zap*)
(*html*
      <p>See the
      $a Tutoeliom.public_session_without_post_params sp <:xmllist< result >> ()$.</p>
      <p>Warning: to implement such connection form, you probably
       get more flexibility using <em>actions</em> instead of xhtml services
       (see below for the same example with actions).
      </p>
      <p>Services registered in session tables are called 
       <em>session</em> or <em>private</em> services. 
       Services registered in the public table
       are called <em>public</em>.
      </p>
    </div>
    <h2>Coservices</h2>
    <div class="twocol1">
      <p>
   A coservice is a service that uses the same URL as 
   a public service, but generates another page.
   They are distinguished from public services only by a special
   parameter, called <em>state</em> parameter.
   Coservices may use GET or POST parameters.</p>
   <p>Most of the time, GET coservices are created dynamically with
   respect to previous interaction with the user and are registered
   in the session table. They allow to give a precise semantics to the
   "back" button of the browser (be sure that you will go back in the
   past) or bookmarks, or duplication of the browser's window.
   (See the calc example below).
   </p>
   <p>
   Use POST coservices if you want to particularize a link or form, 
   but not the URL it points to.
   More precisely, POST coservices are mainly used in two situations:
    </p>
   <ul>
   <li>For the same purpose (new services
   corresponding to precise points of the interaction with the user)
   but when you don't want this service to be bookmarkable.</li>
   <li>To create a button that leads to a service after having performed
   a side-effect. For example a disconnection button that leads to the main
   page of the side, but with the side effect of disconnecting the user.</li>
   </ul>
   <p>
   To create a coservice, use 
   <span class="Cem"><code>new_coservice</code></span> and 
   <span class="Cem"><code>new_post_coservice</code></span>.
   Like <code>register_new_post_service</code>,
   they take a public service as parameter 
   (labelled <code><span class="Clabel">fallback</span></code>)
   to be used as fallback when the user comes back without the state
   parameter (for example if it was a POST coservice and/or the coservice
   has expired).</p>
   <p>The following example shows the difference between GET coservices 
   (bookmarkable) and POST coservices:</p>
*html*)
let coserv = new_service ["co"] unit ()

let coserv2 = 
  new_post_coservice ~fallback:coserv ~post_params:unit ()

let coserv3 = 
  new_coservice ~fallback:coserv ~get_params:unit ()

let _ = 
  let c = ref 0 in
  let page sp () () = 
    let l3 = post_form coserv2 sp 
        (fun _ -> [p [submit_input "incr i (post)"]]) () in
    let l4 = get_form coserv3 sp 
        (fun _ -> [p [submit_input "incr i (get)"]]) in 
    return
      (html
       (head (title (pcdata "")) [])
       (body [p [pcdata "i is equal to ";
                 pcdata (string_of_int !c); br ();
                 a coserv sp [pcdata "reload"] (); br ();
                 a coserv3 sp [pcdata "incr i"] ()];
              l3;
              l4]))
  in
  register coserv page;
  let f sp () () = c := !c + 1; page sp () () in
  register coserv2 f;
  register coserv3 f
(*html*
    </div>
    <div class="twocol2">
      <p>Try $a Tutoeliom.serv sp <:xmllist< <code>coserv</code> >> ()$.</p>
      <p>Note that if the coservice does not exist (for example it
      has expired), the fallback is called.</p>
      <p>In the last example, coservices do not take any parameters
      (but the state parameter), but you can create coservices with
      parameters. Note that the fallback of a GET coservice cannot take
      parameters. (Actually as coservices parameters have special
      names, it is possible to use a "pre-applied" service as fallback
      (see later).</p>

      <div class="encadre">
        <h3>URLs</h3>
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
        <h3>Continuations</h3>
        <p>Eliom is using the concept of <em>continuation</em>. 
        A continuation represents the future of a program (what to do after). 
        When a user clicks on a link or a form, he chooses the future of the
        computation. When he uses the "back" button of the browser, he chooses
        to go back to an old continuation. Continuations for Web programming
        have been introduced by 
        $a (new_external_service
             ["http://www-spi.lip6.fr/~queinnec/PDF/www.pdf"]
             unit unit ()) sp <:xmllist< Christian Queinnec >> ()$,
        and are a big step in
        the understanding of Web interaction.</p>
        <p>
        Some programming languages (Scheme...) allow to manipulate
        continuations using <em>control operators</em> (like 
        <code>call/cc</code>). The style of programming used by Eliom
        is called <em>Continuation Passing Style</em> (CPS), and has the 
        advantage that it does not need control operators, and fits
        very well Web programming.
        </p>
        <p>Coservices allow to create dynamically 
        new continuations that depend on previous interactions with users. 
        Such a behaviour is difficult to simulate with traditional Web
        programming. If you want continuations dedicated to a particular user
        register them in the session table.</p>
      </div>
      <h3>Non-attached coservices</h3>
       <p>
       Non-attached coservices are services that are not attached to an URL.
       When you do a link or a form towards such a service, the URL do not
       change. The name of the service is sent as a hidden parameter.
       </p>
       <p>As for attached coservices, there is a GET and a POST version.
       To create them, use <code>Eliom.new_coservice'</code> or
       <code>Eliom.new_post_coservice'</code>.
       POST non-attached coservices are really usefull if you want a
       link or form to be present on every page but you don't want the
       URL to change. Very often, POST coservices are used with <em>actions</em>
       (see below).
       </p>
    </div>
    <h2>Coservices in session tables</h2>
    <div class="twocol1">
    <p>You can register coservices in session tables to create
       dynamically new services dedicated to an user.
       Here is an example of pages that add two integers.
       Once the first number is sent by the user, a coservice
       is created and registered in the session table. This service
       takes the second number as parameter and displays the result of
       the sum with the first one.
       Try to duplicate the pages and/or to use the back button of your
       navigator to verify that it has the expected behaviour.</p>
*html*)
(*zap* ------------------------------------------------------------------ *)
(* You can register coservices in session tables.
   Use this if you want a link or a form which depends precisely on an
   instance of the web page, for example to buy something on an internet shop.
   UPDATE: Actually it is not a good example, because what we want in a shop
   is the same shoping basket for all pages. 
   SEE calc example instead.
*)
let shop_without_post_params =
  new_service
   ~url:["shop"]
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
        let sb = string_input article in
          <:xmllist< <p> What do you want to buy? $sb$ </p> >>) ())

let shop_public_main_page sp () () =
  let f = write_shop shop_with_post_params sp in return
    << <html><body>$f$</body></html> >>

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
    register_for_session
      sp
      ~service:coshop_with_post_params
      (fun sp () article -> 
                 page_for_shopping_basket 
                   sp (article::shopping_basket));
    register_for_session
      sp
      copay
      (fun sp () () -> return
           << <html><body>
                <p>You are going to pay: 
                  $list:write_shopping_basket shopping_basket$ </p>
              </body></html> >>);
       return << <html>
           <body> 
             <div>$list:write_shopping_basket shopping_basket$</div>
             $write_shop coshop_with_post_params sp$ 
             $post_form copay sp 
                    (fun _ -> [p [submit_input "pay"]]) ()$
           </body>
         </html> >>

let _ = register
  ~service:shop_with_post_params
  (fun sp () article -> page_for_shopping_basket sp [article])
(* *zap*)
(*zap* Queinnec example: *zap*)
let calc = 
  new_service
    ~url:["calc"]
    ~get_params:unit
    ()

let calc_i = 
  new_service 
    ~url:["calc"]
    ~get_params:(int "i")
    ()

let _ = 
  let create_form is = 
    (fun entier ->
       [p [pcdata (is^" + ");
           int_input entier;
           br ();
           submit_input "Sum"]])
  in
  register
    ~service:calc_i
    (fun sp i () ->
      let is = string_of_int i in
      let calc_result = 
	register_new_coservice_for_session
          sp
          ~fallback:calc
          ~get_params:(int "j")
          (fun sp j () -> 
            let js = string_of_int j in
            let ijs = string_of_int (i+j) in  return
            (html
               (head (title (pcdata "")) [])
               (body
                  [p [pcdata (is^" + "^js^" = "^ijs)]])))
      in
      let f = 
        get_form calc_result sp (create_form is) in 
      return
        (html
	   (head (title (pcdata "")) [])
	   (body [f])))
(*html*
    </div>
    <div class="twocol2">
*html*)    
let _ =   
  let create_form = 
    (fun entier ->
      [p [pcdata "Write a number: ";
          int_input entier;
          br ();
          submit_input "Send"]])
  in
  register
    calc
    (fun sp () () ->
      let f = get_form calc_i sp create_form in
      return
        (html 
	   (head (title (pcdata "")) [])
	   (body [f])))
(*html*
      <p>See the $a Tutoeliom.calc sp <:xmllist< result >> ()$.</p>
    </div>





    <h2>Actions</h2>
    <div class="twocol1">
      <p>Actions are like services but they do not generate any page.
   Use them to perform an effect on the server (connection/disconnection
   of a user, etc.). The page you link to is redisplayed after the action.
   For ex, when you have the same form (or link) on several pages 
   (for ex a connection form),
   instead of making a version with post params of all these pages,
   you can use only one action, registered on a non-attached coservice.
   To register actions, just use the module <code>Actions</code>
   instead of <code>Eliom.Xhtml</code> or <code>Eliomduce</code>.
   For example
     <code><span class="Cem">Actions.register</span></code>,
     <code><span class="Cem">Actions.register_new_service</span></code>,
     <code><span class="Cem">Actions.register_for_session</span></code>.<br/>
      </p>
      <p>Here we rewrite the example <code>session</code> using actions
      (and a POST coservice for disconnection).</p>
*html*)
let action_session = 
  new_service ~url:["action"] ~get_params:unit ()

let connect_action = 
  new_post_coservice' ~post_params:(string "login") ()
    
let accueil_action sp () () = 
  let f = post_form connect_action sp
      (fun login -> 
        [p [pcdata "login: "; 
            string_input login]]) () in 
  return
    (html
       (head (title (pcdata "")) [])
       (body [f]))
    
let _ = register
  ~service:action_session
  accueil_action
(*html*
    </div>
    <div class="twocol2">
*html*)
let rec launch_session sp login =
  let disconnect_action = 
   Actions.register_new_post_coservice_for_session'
      sp
      unit 
      (fun sp () () -> close_session sp; return []) in
  let disconnect_box sp s = 
    post_form disconnect_action sp 
      (fun _ -> [p [submit_input s]]) ()
  in
  let new_main_page sp () () = return
    (html
      (head (title (pcdata "")) [])
      (body [p [pcdata "Welcome ";
                pcdata login; br ();
                a coucou sp [pcdata "coucou"] (); br ();
                a hello sp [pcdata "hello"] (); br ();
                a links sp [pcdata "links"] (); br ()];
             disconnect_box sp "Close session"]))
  in
  register_for_session 
    sp ~service:action_session new_main_page;
  register_for_session sp coucou
   (fun _ () () -> return
     (html
       (head (title (pcdata "")) [])
       (body [p [pcdata "Coucou ";
                 pcdata login;
                 pcdata "!"]])));
  register_for_session sp hello 
   (fun _ () () -> return
     (html
       (head (title (pcdata "")) [])
       (body [p [pcdata "Ciao ";
                 pcdata login;
                 pcdata "!"]])))
    
let _ = Actions.register
    connect_action
    (fun sp () login -> 
      launch_session sp login;
      return [])
(*html*
      <p>See these $a Tutoeliom.action_session sp <:xmllist< pages >> ()$.</p>
    </div>
    <h2>Other concepts</h2>
    <div class="twocol1">
    <h3>Pre-applied services</h3>
    <p>Services or coservices with GET parameters can be preapplied
     to obtain a service without parameters. Example:
    </p>
    <pre>
let preappl = preapply coucou_params (3,(4,"cinq"))
    </pre>
    <p>
     It is not possible to register something on e preapplied service,
     but you can use them in links or as fallbacks for coservices.
    </p>
    <h3>Giving informations to fallbacks</h3>
    <p>Fallbacks have access to some informations about what succeeded but
    they were called. Get this information using 
     <code>Eliom.get_exn sp</code>; That function returns a list of exceptions.
    That list contains <code>Eliom_link_to_old</code> if the coservice
    was not found.
    </p>
    <p>
    It is also possible to tell actions to send informations to the page
    generated after them. Just place exceptions in the list returned by the
    action. These exceptions will also be accessible with 
    <code>Eliom.get_exn</code>. Try to replace the lines above by:
    </p>
*html*)
(*zap* *)
let action_session2 = 
  new_service ~url:["action2"] ~get_params:unit ()

let connect_action = 
  new_post_coservice' ~post_params:(string "login") ()
(* *zap*)    
exception Bad_user
let login_box sp login =
   let l =
     [pcdata "login: "; 
      string_input login]
   in
   [p (if List.mem Bad_user (get_exn sp)
       then (pcdata "Wrong user")::(br ())::l
       else l
   )]

let accueil_action sp () () = 
  let f = post_form connect_action sp (login_box sp) () in
  return
    (html
       (head (title (pcdata "")) [])
       (body [f]))

(*zap* *)    
let _ = register
  ~service:action_session2
  accueil_action

let rec launch_session sp login =
  let disconnect_action = 
   Actions.register_new_post_coservice_for_session'
      sp
      unit 
      (fun sp () () -> close_session sp; return []) in
  let disconnect_box sp s = 
    post_form disconnect_action sp 
      (fun _ -> [p [submit_input s]]) ()
  in
  let new_main_page sp () () = return
    (html
      (head (title (pcdata "")) [])
      (body [p [pcdata "Welcome ";
                pcdata login; br ();
                a coucou sp [pcdata "coucou"] ()];
             disconnect_box sp "Close session"]))
  in
  register_for_session 
    sp ~service:action_session2 new_main_page;
  register_for_session sp coucou
   (fun _ () () -> return
     (html
       (head (title (pcdata "")) [])
       (body [p [pcdata "Coucou ";
                 pcdata login;
                 pcdata "!"]])))
    
(* *zap*)
let _ = Actions.register
    connect_action
    (fun sp () login -> 
      if login = "toto" 
      then (launch_session sp login; return [])
      else return [Bad_user])
(*html*
    <h3>Redirections</h3>
    <p>
     The <code>Redirections</code> module allows to register HTTP redirections.
     If a request is done towards such a service, the server asks the browser
     to retry with another URL. Example:
    </p>
*html*)
let redir = Redirections.register_new_service
    ~url:["redir"]
    ~get_params:(int "o")
   (fun sp o () -> return (make_string_uri coucou_params sp (o,(22,"ee"))))
(*html*
      <p>Try $a Tutoeliom.redir sp <:xmllist< <code>it</code> >> 11$.</p>
     <h3>Sending files</h3>
      <p>You may want to register a service that will send a file.
      To do that, use the <code>Files</code> module. Example:
      </p>
<pre>
let sendfile = 
  Files.register_new_service 
    ~url:["sendfile"]
    ~get_params:unit
    (fun _ () () -&gt; return "filename")
</pre>
      <p>Other example, with suffix URL:
      </p>
<pre>
let sendfile2 = 
  Files.register_new_service 
    ~url:["files"]
    ~get_params:(suffix_only "suf")
    (fun _ s () -&gt; return ("<em>path</em>"^(string_of_url_path s)))
</pre>
      <p>The extension <code>Staticmod</code> is another way to
       handle static files (see the default 
       configuration file for more informations).
      </p>
     <h3>Registering services that decide what they want to send</h3>
      <p>You may want to register a service that will send sometimes
      an xhtml page, sometimes a file, sometimes something else.
      To do that, use the <code>Any</code> module, together
      with the <code>send</code> function of the module you want
      to use. Example:
      </p>
*html*)
let sendany = 
  Any.register_new_service 
    ~url:["sendany"]
    ~get_params:(string "type")
   (fun sp s () -> 
     if s = "valid"
     then
       return
         (Xhtml.send sp
           (html
             (head (title (pcdata "")) [])
             (body [p [pcdata "This page has been statically typechecked. \
                               If you change the parameter in the URL you \
                               will get an unchecked text page"]])))
     else 
       return
         (Text.send sp "<html><body><p>It is not a valid page. Put \
                     type=\"valid\" in the URL to get a typechecked page.\
                     </p></body></html>")
   )
(*html*
      <p>You may also use <code>Any</code> to send cookies or to choose a
         different charset than the default 
        (default charset is set in configuration file) 
         for the page you send. To do that use the optional parameters
          <code>?cookies</code> and <code>?charset</code>.
      </p>
    </div>
    <div class="twocol2">
     <h3>Cookies</h3>
     <p>
      You can set cookies on the client, by using functions like
      <code>Cookies.register</code> instead of <code>register</code>.
      The function you register returns a pair containing the page as usual
      and a list of cookies, of type
      </p>
      <pre>
type cookieslist = (string option * (string * string) list) list
</pre>
     <p>
     The <code>string option</code> is a the path for which you want
     to set the cookie (relative to the main directory of your site, defined
     in the configuration file). <code>None</code> means for all your site.
     </p>
     <p>
      You can access the cookies sent by the browser using
      <code>Eliom.get_cookies sp</code>.
     </p>
     <p>
      Example:
     </p>
*html*)
let cookiename = "mycookie"

let cookies = new_service ["cookies"] unit ()

let _ = Cookies.register cookies
    (fun sp () () ->  return
      ((html
        (head (title (pcdata "")) [])
        (body [p [pcdata (try
                            "cookie value: "^
                            (List.assoc cookiename (get_cookies sp))
                          with _ -> "<cookie not set>");
                  br ();
                  a cookies sp [pcdata "send other cookie"] ()]])),
       [None, [(cookiename,(string_of_int (Random.int 100)))]]))
(*html*
      <p>Try $a Tutoeliom.cookies sp <:xmllist< <code>it</code> >> ()$.</p>
     <h3>Disposable coservices</h3>
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
          (body [p [(if List.mem Eliom.Eliom_link_to_old (get_exn sp)
                    then pcdata "Your link was outdated. I am the fallback. \
                            I just created a new disposable coservice. \
                            You can use it only twice."
                    else
                    pcdata "I just created a disposable coservice. \
                            You can use it only twice.");
                    br ();
                    a disp_coservice sp [pcdata "Try it!"] ()]])))
(*html*      
      <p>Try $a Tutoeliom.disposable sp <:xmllist< <code>it</code> >> ()$.</p>
     <h3>Timeout for sessions</h3>
      <p>The default timeout for sessions in one hour. Sessions will be
       automatically closed after that amount of time of inactivity
       from the user.
       You can change that value for your whole site during initialisation 
       using:</p>
<pre>
set_global_timeout_during_init (Some 7200.)
</pre>
      <p>Here 7200 seconds. <code>None</code> means no timeout.</p>
      <p>
       You can change that value for your whole site after initialisation 
       using:</p>
<pre>
set_global_timeout_during_session sp (Some 7200.)
</pre>
      <p>
       You can change that value for one user only using:</p>
<pre>
set_user_timeout sp (Some 7200.)
</pre>
      <p>
      Note that there is also a possibility to change the default value
      for Eliom in the configuration file like this:</p>
<pre>
    &lt;dynlink module="<em>path_to</em>/eliom.cma"&gt;
      &lt;timeout value="7200"/&gt;
    &lt;/dynlink&gt;
</pre>
     <p><code>value="infinity"</code> means no timeout.</p>
     <p>Warning: that default may be overriden by each site using 
        <code>set_global_timeout_during_...</code>.
        If you want your user to be able to set the default in the 
        configuration file for your site, you must parse the configuration
        (<code>Eliom.get_config ()</code> function).
     </p>
     <h3>Giving configuration options to your sites (EXPERIMENTAL)</h3>
      <p>You can add your own options in the configuration
       file for your Web site. For example:</p>
<pre>
    &lt;eliom module="<em>path_to</em>/yourmodule.cmo"&gt;
      &lt;youroptions&gt; ...
    &lt;/eliom&gt;
</pre>
      <p>
       Use <code>Eliom.get_config ()</code> to get the data between
       <code>&lt;eliom&gt;</code> and <code>&lt;/eliom&gt;</code>.
       Warning: parsing these data is very basic for now and not really easy.
       That feature will be improved in the future.
      </p>
    </div>
    <h2>Threads</h2>
    <div class="twocol1">
      <p>
      Remember that a Web site written with Eliom is an OCaml application.
      This application must be able to handle several requests at the same 
      time, if one of the requests takes time. To make this possible, Ocsigen
      is using <em>cooperative threads</em>, 
      implemented in monadic style
      by Jérôme Vouillon (<code>Lwt</code> module), which make them really easy
      to use.
      </p>
      <p>With respect to preemptive threads, cooperative threads are not using
      a scheduler to distribute processor time between threads. Instead of 
      this, each thread must tell the others that he wants to let them
      work. If a thread does not cooperate, 
        the others will be blocked.
      </p>
      <dl>
        <dt>Advantages</dt><dd><ul>
          <li>It is much lighter</li>
          <li>No need of mutex and no risk of deadlock!</li>
          <li>The use of many (small) threads make implementation very easy (for example, for user interfaces, no need to implement another event loop, make a thread for each widget!)</li>
         </ul></dd>
        <dt>Drawbacks</dt><dd><ul>
          <li>Threads must cooperate&nbsp;... Otherwise the whole program will hang.</li></ul></dd>
      </dl>
      <p>As it does not cooperate, the following page will stop the
      server for 5 seconds. No one will be able to do a request during
      this delay:</p>
<pre><span style="color:green">let</span> looong =
  register_new_service
    <span style="color:#770000">~url:</span>[<span style="color:#aa4444">"looong"</span>]
    <span style="color:#770000">~get_params:</span>unit
    (<span style="color:green">fun</span> sp () () -&gt;
      <span style="color:#0033cc">Unix</span>.sleep 5;
      return
        (html
          (head (title (pcdata <span style="color:#aa4444">""</span>)) [])
          (body [h1 [pcdata <span style="color:#aa4444">"Ok now, you can read the page."</span>]])))</pre>
      <p>To solve this problem, use a cooperative version of 
         <code>sleep</code>:</p>
*html*)
let looong = 
  register_new_service 
    ~url:["looong"]
    ~get_params:unit
    (fun sp () () -> 
      Lwt_unix.sleep 5.0 >>= (fun () ->
        return
        (html
          (head (title (pcdata "")) [])
          (body [h1 [pcdata 
	           "Ok now, you can read the page."]]))))
(*html*
     <p>The <code>&gt;&gt;=</code> operator (from <code>Lwt</code>) is used to
     specify a sequence of computations that depend one from another.
     It is a kind of <code>let</code> binding.
     <code>e1 &gt;&gt;= (fun r -&gt; return e2)</code>
     will try to evaluate <code>e1</code>, and once <code>e1</code>
     is evaluated, it will give the result to the function given as second
     parameter.
     If the left handside (<code>e1</code>)
     takes time (for example because it is waiting for a read on a socket),
     the whole computation will be saved in a table and the program will
     continue to the next instruction that does not depend on <code>e1</code>. 
     The computation will resume at a future
     cooperation point, if it is ready to continue.
     Instead of <code>e1 &gt;&gt;= (fun r -&gt; return e2)</code>,
     you can write <code>bind e1 (fun r -&gt; return e2)</code>.
     </p>
     <p>See $a Tutoeliom.looong sp <:xmllist< looong >> ()$.</p>
     <p><code>Lwt.bind</code>, (or <code>&gt;&gt;=</code>) has type<br/>
        <code>'a Lwt.t -&gt; ('a -&gt; 'b Lwt.t) -&gt; 'b Lwt.t</code></p>
     <p><code>Lwt.return</code> has type<br/>
        <code>'a -&gt; 'a Lwt.t</code></p>
     <p><code>'a Lwt.t</code> is the type of threads returning 
        a result of type <code>'a</code>.</p>
     <p>Cooperation points are inserted when you call cooperative functions
     such as <code>Lwt_unix.read</code> or <code>Lwt_unix.write</code>.
     You can add other cooperation points by calling
     <code>Lwt_unix.yield ()</code>. The thread will suspend itself,
     let other threads run, and resume as soon as possible.
     </p>
    </div>
    <div class="twocol2">
     <h3>Catching exceptions</h3>
     <p>You must be careful when catching exception with <code>Lwt</code>.
     If you use the <code>try ... with</code> construct for an expression
     of type <code>'a Lwt.t</code>, it may not work (as the computation
     may happen later).</p>
     <p>Remember the following: if e has type <code>'a Lwt.t</code> 
      (where <code>'a</code> is any type), do not write:</p>
<pre><span style="color:#77aaaa">try</span>
  e
<span style="color:#77aaaa">with</span>
  ...</pre>
     <p>but write:</p>
<pre>catch
  (<span style="color:green">fun</span> () -&gt; e)
  (<span style="color:green">function</span> ... <span style="color:#77aaaa">|</span> exn -&gt; fail exn)</pre>
     <h3>What if my function is not implemented in cooperative way?</h3>
      <h4>If my function is thread-safe (for preemptive threads)</h4>
      <p>Ocsigen implements a way to make a non cooperative computation be
      executed automatically by a another preemptive thread (for example
      a database request using a non-cooperative database library, such as 
      postgresql-ocaml or pgocaml). To do this,
      use the <code>detach</code> function. For example:</p>
*html*)
let looong2 = 
  register_new_service 
    ~url:["looong2"]
    ~get_params:unit
    (fun sp () () -> 
      (Preemptive.detach Unix.sleep 5) >>= (fun () ->
        return
        (html
          (head (title (pcdata "")) [])
          (body [h1 [pcdata 
		   "Ok now, you can read the page."]]))))
(*html*
      <p>See $a Tutoeliom.looong2 sp <:xmllist< looong2 >> ()$.</p>      
      <p>A pool of preemptive threads is waiting for such 
      "detached functions". You can specify the number of threads in the pool
      in the configuration file.</p>
      <p>Warning: Detached functions must be thread-safe! Be careful to
      concurrent access to data. Be sure to use mutexes for your own functions,
      and use only thread-safe libraries.<!-- For example <code></code>
      (version ) is NOT thread-safe, <code></code>
      (version ) is thread-safe. --> The libraries from Ocsigen
      are NOT thread-safe for now. Let us know if you need them to be
      thread-safe.</p>
      <h4>If my function is not thread-safe (for preemptive threads)</h4>
      <p>If you want to use a function that takes time to execute but
      it not written in thread-safe way, consider rewriting it in cooperative
      manner, or delegate the work to another process.</p>
     <h3>Examples</h3>
      <h4>A thread that prints "hello" every 10 seconds</h4>
      <p>Just add the following lines to your program:</p>
      <pre><span style="color:green">let rec</span> f () = 
  print_endline "hello";
  <span style="color:#0033cc">Lwt_unix</span>.sleep 10. &gt;&gt;= f
in f ();
      </pre>
      <h4>Create a thread waiting for an event</h4>
      <pre><span style="color:green">let</span> w = wait () in
(w &gt;&gt;= (<span style="color:green">fun</span> v -&gt; return (print_endline v));
...
wakeup w "HELLO");
      </pre>
    </div>
    <h2>Static parts</h2>
    <div class="twocol1">
      <h3>Fully static pages</h3>
      <p>With <code>staticmod</code>, you can associate a static directory
         where you can put all the static (non generated) parts of your 
         web-site (for examples images).
         See the default config file <code>ocsigen.conf</code> to
         learn how to do that.
         There is a predefined service called 
         <code><span class="Cem">static_dir</span></code> to make links to
            static files. It takes as string parameter the name of the file.
                <br/>
                For example</p>
         <pre><span class="Cconstructor">Eliom</span>.a 
  (static_dir sp)
  sp
  [pcdata "download image"] 
  "$str:small_logo$"</pre>
          <p>creates this link:
         $a (static_dir sp) sp [pcdata "download image"] small_logo$
      </p>
      <p>It is now also possible to handle static pages yourself using
      <code>Eliom.Files</code>.
      </p>
      <!-- h3>Static parts of a page</h3>
      <em>To be available soon</em -->
    </div>
    <div class="twocol2">
    </div>
    <h2>Predefined constructs</h2>
    <div class="twocol1">
      <h3>Images, CSS, Javascript</h3>
      <p>
      To include an image, use simply the function <code>XHTML.M.img</code>:
      </p>
      <pre>img <span class="Clabel">~alt:</span>"Ocsigen" 
    <span class="Clabel">~src:</span>(<span class="Cem">make_uri</span> (static_dir sp) sp <span class="Cstring">"ocsigen1024.jpg"</span>)
    ()</pre>
      <p>The function <code><span class="Cem">make_uri</span></code>
        creates the relative URL string from current URL (in <code>sp</code>)
        (see above) to the URL of the image in the static directory
        configured in the configuration file.
      </p>
      <p>To simplify the creation of <code>&lt;link&gt;</code> tags
      for CSS or <code>&lt;script&gt;</code> tags for Javascript,
        use the following functions:</p>
      <pre><span class="Cem">css_link</span> (make_uri (static_dir sp) sp <span class="Cstring">"style.css"</span>)</pre>
      <pre><span class="Cem">js_script</span> (make_uri (static_dir sp) sp <span class="Cstring">"funs.js"</span>)</pre>
      <h3>Menus</h3>
      <p>
      To make a menu an your web page, you can use the function 
          <code><span class="Cem">Eliomboxes.menu</span></code>.
      First, define your menu like this:
      </p>
<pre><span class="Clet">let</span> mymenu current sp <span class="Cnonalphakeyword">=</span>
  <span class="Cconstructor">Eliomboxes</span>.menu <span class="Clabel">~classe:</span><span class="Cnonalphakeyword">[</span><span class="Cstring">"menuprincipal"</span><span class="Cnonalphakeyword">]</span>
    <span class="Cnonalphakeyword">(</span>home<span class="Cnonalphakeyword">,</span> &lt;:xmllist<span class="Cnonalphakeyword">&lt;</span> Home &gt;&gt;<span class="Cnonalphakeyword">)</span>
    <span class="Cnonalphakeyword">[</span>
     <span class="Cnonalphakeyword">(</span>infos<span class="Cnonalphakeyword">,</span> &lt;:xmllist<span class="Cnonalphakeyword">&lt;</span> More infos &gt;&gt;<span class="Cnonalphakeyword">)</span><span class="Cnonalphakeyword">;</span>
     <span class="Cnonalphakeyword">(</span>tutorial<span class="Cnonalphakeyword">,</span> &lt;:xmllist<span class="Cnonalphakeyword">&lt;</span> Documentation &gt;&gt;<span class="Cnonalphakeyword">)</span>
   <span class="Cnonalphakeyword">]</span> current sp</pre>
      <p>Here, <code>home</code>,  <code>infos</code>, 
        and <code>tutorial</code> are your three pages (generated for example
        by <code>Eliom.new_service</code>).</p>
    </div>
    <div class="twocol2">
      <p>Then <code>mymenu home sp</code> will generate the following
        code:</p>
      <pre>&lt;ul class="menu menuprincipal"&gt;
  &lt;li class="current first"&gt;Home
  &lt;/li&gt;
  &lt;li&gt;&lt;a href="infos"&gt;More infos&lt;/a&gt;
  &lt;/li&gt;
  &lt;li class="last"&gt;&lt;a href="tutorial"&gt;Documentation&lt;/a&gt;
  &lt;/li&gt;
&lt;/ul&gt;</pre>
    <p>Personalise it in your CSS style-sheet.</p>
    <p><code>Eliomboxes.menu</code> takes a list of services without
    GET parameters. 
    If you want one of the link to contains GET parameters, pre-apply
    the service.</p>
      <div class="encadre">
        <h3>Tip: How to make a menu with different kinds of urls 
   (external, internal...)?</h3>
          <p>
   You need to coerce each of them. For example</p>
       <pre>
 [(home :> (('a,'b,Eliom.service_kind,'c,'d,'e) service))]
       </pre>
      </div>
      <div class="encadre">
        <h3>How to make a menu entry with GET parameters?</h3>
          <p>
          Preapply your service.
          </p>
      </div>
      <h3>Others</h3>
      <em>To be available soon</em>
    </div>
    <h2>Example: write a forum</h2>
    <div class="twocol1">
      As an example,
      we will now write a small forum. Our forum has a main page,
      summarising all the messages and a page for each message.
      Suppose you have written a function <code>news_headers_list_box</code>
      that writes the beginning of messages, and <code>message_box</code>
      that write a full message.
*html*)
(*zap* from ocsexample1 - attention la section Construction of pages a été simplifiée *zap*)
(*html*
<pre>
<span class="Ccomment">(* All the services: *)</span>

<span class="Clet">let</span> main_page <span class="Cnonalphakeyword">=</span> new_service <span class="Clabel">~url:</span><span class="Cnonalphakeyword">[</span><span class="Cstring">""</span><span class="Cnonalphakeyword">]</span>
    <span class="Clabel">~get_params:</span>unit <span class="Cnonalphakeyword">(</span><span class="Cnonalphakeyword">)</span>

<span class="Clet">let</span> news_page <span class="Cnonalphakeyword">=</span> new_service <span class="Cnonalphakeyword">[</span><span class="Cstring">"msg"</span><span class="Cnonalphakeyword">]</span> <span class="Cnonalphakeyword">(</span><span class="Cconstructor">StringMessage</span><span class="Cnonalphakeyword">.</span>index <span class="Cstring">"num"</span><span class="Cnonalphakeyword">)</span> <span class="Cnonalphakeyword">(</span><span class="Cnonalphakeyword">)</span>

<span class="Ccomment">(* Construction of pages *)</span>

<span class="Clet">let</span> accueil sp () () <span class="Cnonalphakeyword">=</span>
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
  accueil

<span class="Clet">let</span> <span class="Cnonalphakeyword">_</span> <span class="Cnonalphakeyword">=</span> register
  <span class="Clabel">~service:</span>news_page
  print_news_page
</pre>
    </div>
    <div class="twocol2">
      <p>Now the same with a login box on each page.
      We now have two versions of each page: connected and not connected.
      We need two actions (for connection and disconnection). 
      Suppose we have the functions <code>login_box</code>,
      <code>connected_box</code>,
      and <code>connect</code>.
      </p>
*html*)
(*zap* from ocsexample2 *zap*)
(*html*
<pre><span class="Ccomment">(* All the urls: *)</span>

<span class="Clet">let</span> main_page <span class="Cnonalphakeyword">=</span> new_service <span class="Clabel">~url:</span><span class="Cnonalphakeyword">[</span><span class="Cstring">""</span><span class="Cnonalphakeyword">]</span> <span class="Clabel">~get_params:</span>unit <span class="Cnonalphakeyword">(</span><span class="Cnonalphakeyword">)</span>

<span class="Clet">let</span> news_page <span class="Cnonalphakeyword">=</span> new_service <span class="Cnonalphakeyword">[</span><span class="Cstring">"msg"</span><span class="Cnonalphakeyword">]</span> <span class="Cnonalphakeyword">(</span><span class="Cconstructor">StringMessage</span><span class="Cnonalphakeyword">.</span>index <span class="Cstring">"num"</span><span class="Cnonalphakeyword">)</span> <span class="Cnonalphakeyword">(</span><span class="Cnonalphakeyword">)</span>

<span class="Clet">let</span> connect_action <span class="Cnonalphakeyword">=</span>
  new_post_coservice'
    <span class="Clabel">~post_params:</span><span class="Cnonalphakeyword">(</span>string <span class="Cstring">"login"</span> ** string <span class="Cstring">"password"</span>
<span class="Cnonalphakeyword">)</span>

<span class="Ccomment">(* Construction of pages *)</span>

<span class="Clet">let</span> accueil sp () () <span class="Cnonalphakeyword">=</span>
  page sp
    <span class="Cnonalphakeyword">[</span>h1 [pcdata <span class="Cstring">"Mon site"</span>]<span class="Cnonalphakeyword">;</span>
     p [pcdata <span class="Cstring">"(user : toto and password : titi)"</span>]<span class="Cnonalphakeyword">;</span>
     login_box sp connect_action<span class="Cnonalphakeyword">;</span>
     news_headers_list_box sp anonymoususer news_page<span class="Cnonalphakeyword">]</span>

<span class="Clet">let</span> print_news_page sp i () <span class="Cnonalphakeyword">=</span> 
  page sp
    <span class="Cnonalphakeyword">[</span>h1 [pcdata <span class="Cstring">"Info"</span><span class="Cnonalphakeyword">;</span>
     login_box sp connect_action<span class="Cnonalphakeyword">;</span>
     message_box i anonymoususer<span class="Cnonalphakeyword">]</span>

<span class="Clet">let</span> user_main_page user sp () () <span class="Cnonalphakeyword">=</span>
  page sp
    <span class="Cnonalphakeyword">[</span>h1 [pcdata <span class="Cstring">"Mon site"</span>]<span class="Cnonalphakeyword">;</span>
     text_box <span class="Cstring">"Bonjour !"</span><span class="Cnonalphakeyword">;</span>
     connected_box sp user disconnect_action<span class="Cnonalphakeyword">;</span>
     news_headers_list_box sp user news_page<span class="Cnonalphakeyword">]</span>

<span class="Clet">let</span> user_news_page user sp i () <span class="Cnonalphakeyword">=</span> 
  page sp
    <span class="Cnonalphakeyword">[</span>h1 [pcdata <span class="Cstring">"Info"</span>]<span class="Cnonalphakeyword">;</span>
     connected_box sp user disconnect_action<span class="Cnonalphakeyword">;</span>
     message_box i user<span class="Cnonalphakeyword">]</span>

<span class="Ccomment">(* Services registration *)</span>

<span class="Clet">let</span> <span class="Cnonalphakeyword">_</span> <span class="Cnonalphakeyword">=</span> register
  <span class="Clabel">~service:</span>main_page
  accueil

<span class="Clet">let</span> <span class="Cnonalphakeyword">_</span> <span class="Cnonalphakeyword">=</span> register
  <span class="Clabel">~service:</span>news_page
  print_news_page

<span class="Clet">let</span> launch_session user <span class="Cnonalphakeyword">=</span>
  register_for_session <span class="Clabel">~service:</span>main_page <span class="Cnonalphakeyword">(</span>user_main_page user<span class="Cnonalphakeyword">)</span><span class="Cnonalphakeyword">;</span>
  register_for_session <span class="Clabel">~service:</span>news_page <span class="Cnonalphakeyword">(</span>user_news_page user<span class="Cnonalphakeyword">)</span>

<span class="Clet">let</span> <span class="Cnonalphakeyword">_</span> <span class="Cnonalphakeyword">=</span> Actions.register
  <span class="Clabel">~action:</span>connect_action
    <span class="Cnonalphakeyword">(</span><span class="Cfun">fun</span> h <span class="Cnonalphakeyword">(</span>login<span class="Cnonalphakeyword">,</span> password<span class="Cnonalphakeyword">)</span> <span class="Cnonalphakeyword">-&gt;</span>
      launch_session <span class="Cnonalphakeyword">(</span>connect login password<span class="Cnonalphakeyword">)</span>; return []<span class="Cnonalphakeyword">)</span>
</pre>




    </div>
*html*)
(*zap* À AJOUTER AU TUTO *)





(* ------------------------------------------------------------------ *)
(* Advanced types *)
(* You can use your own types *)


(* lists *)
let coucou_list = register_new_service 
    ~url:["coucou"]
    ~get_params:(list "a" (string "str"))
  (fun _ l () ->
    let ll = 
      List.map (fun s -> << <strong>$str:s$</strong> >>) l in  return
  << <html>
       <head><title></title></head>
       <body>
       <p>
         You sent: 
         $list:ll$
       </p>
       </body>
     </html> >>)

(* http://localhost:8080/coucou?a=2&a.entier[0]=6&a.entier[1]=7 *)

(* Advanced forms *)
(* Form with list: *)
let create_listform f = 
  (* Here, f.it is an iterator like List.map, 
     but it must be applied to a function taking 2 arguments 
     (and not 1 as in map), the first one being the name of the parameter.
     The last parameter of f.it is the code that must be appended at the 
     end of the list created
   *)
  f.it (fun stringname v ->
    <:xmllist< <p>Write the value for $str:v$: $string_input stringname$ </p> >>)
    ["one";"two";"three";"four"]
    <:xmllist< <p>$submit_input "Click"$</p> >>

let listform = register_new_service ["listform"] unit
  (fun sp () () -> 
     let f = get_form coucou_list sp create_listform in return
     << <html>
          <head><title></title></head>
          <body> $f$ </body>
        </html> >>)

(* Form for service with suffix: *)
let create_suffixform (suff,i) =
    <:xmllist< <p>Write the suffix: 
      $user_type_input string_of_url_path suff$ <br/>
      Write an int: $int_input i$ <br/>
      $submit_input "Click"$</p> >>

let suffixform = register_new_service ["suffixform"] unit
  (fun sp () () -> 
     let f = get_form isuffix sp create_suffixform in return
     << <html>
          <head><title></title></head>
          <body> $f$ </body>
        </html> >>)

(* Form with checkbox: *)
let bool_params = register_new_service 
    ~url:["bool"]
    ~get_params:(bool "case")
  (fun _ case () -> return
  << <html>
       <head><title></title></head>
       <body>
       <p>
         $pcdata (if case then "checked" else "not checked")$
       </p>
       </body>
     </html> >>)

let create_form_bool casename =
    <:xmllist< <p>check? $bool_checkbox casename$ <br/>
      $submit_input "Click"$</p> >>

let form_bool = register_new_service ["formbool"] unit
  (fun sp () () -> 
     let f = get_form bool_params sp create_form_bool in return
     << <html>
          <head><title></title></head>
          <body> $f$ </body>
        </html> >>)


(* Other Eliom module: *)
let coucoutext = 
  Eliom.Text.register_new_service 
    ~url:["coucoutext"]
    ~get_params:unit
    (fun sp () () -> return
      ("<html>n'importe quoi "^(Eliom.Text.a coucou sp "clic" ())^"</html>"))


(* Fin À AJOUTER *)
(* Main page for this example *)
let main = new_service [] unit ()

let _ = register main
  (fun sp () () -> return
    (* Do not register a page after initialisation.
       This will cause an error:
       let coucou6 = 
       new_service 
        ~url:["coucou6"]
        ~server_params:no_server_param
        ~get_params:no_get_param 
        ()
       in *)
    (* This will be ignored: register coucou1 << <html></html> >>; *)
     << 
       <html> 
       <!-- This is a comment! -->
       <head>
         $css_link (make_uri (static_dir sp) sp ["style.css"])$
         <title>Eliom Tutorial</title>
       </head>
       <body>
       <h1>Eliom</h1>
       <h2>Examples</h2>
       <h3>Simple pages</h3>
       <p>
         A simple page: $a coucou sp <:xmllist< coucou >> ()$ <br/>
         A page with a counter: $a compt sp <:xmllist< compt >> ()$ <br/> 
         A page in a directory: 
           $a hello sp <:xmllist< dir/hello >> ()$ <br/>
       Default page of a directory:
           $a default sp <:xmllist< rep/ >> ()$</p>
       <h3>Parameters</h3>
       <p>
         A page with GET parameters: 
           $a coucou_params sp <:xmllist< coucou with params >> (45,(22,"krokodile"))$ (what if the first parameter is not an integer?)<br/> 
         A page with "suffix" URL that knows the IP and user-agent of the client: 
           $a uasuffix sp <:xmllist< uasuffix >> (["suf"], "toto")$ <br/> 
         A page with "suffix" URL and GET parameters : 
           $a isuffix sp <:xmllist< isuffix >> (["popo"], 333)$ <br/> 
         A page with a parameter of user-defined type : 
             $a mytype sp <:xmllist< mytype >> A$ </p>
       <h3>Links and Formulars</h3>
       <p>
         A page with links: $a links sp <:xmllist< links >>  ()$ <br/> 
         A page with a link towards itself: 
             $a linkrec sp <:xmllist< linkrec >> ()$ <br/>
         The $a main sp <:xmllist< default page >> ()$ 
             of this directory (myself) <br/>
         A page with a GET form that leads to the "coucou" page with parameters: 
             $a form sp <:xmllist< form >> ()$ <br/> 
         A POST form towards the "post" page: 
             $a form2 sp <:xmllist< form2 >> ()$ <br/> 
         The "post" page, when it does not receive parameters: 
             $a no_post_param_service sp <:xmllist< post wihtout post_params >> ()$ <br/> 
         A POST form towards a service with GET parameters: 
             $a form3 sp <:xmllist< form3 >> ()$ <br/> 
         A POST form towards an external page: 
             $a form4 sp <:xmllist< form4 >> ()$ </p> 
       <h3>Sessions</h3>
       <p>
         Coservices: 
             $a coserv sp <:xmllist< coservice >> ()$ <br/> 
         A session based on cookies: 
             $a public_session_without_post_params sp <:xmllist< session >> ()$ <br/> 
         A session based on cookies, implemented with actions: 
             $a action_session sp <:xmllist< actions >> ()$ <br/>
         The same with wrong user if not "toto": 
             $a action_session2 sp <:xmllist< actions2 >> ()$ <br/>
         Auxuiliary services in the session table:
             $a calc sp <:xmllist< calc >> ()$
       <!--  (ancienne version : $a shop_without_post_params sp <:xmllist< shop >> ()$) -->
       </p>
       <h3>Other</h3>
       <p>
       A page that is very slow, implemented in cooperative way: 
             $a looong sp <:xmllist< looong >> ()$<br/>
       A page that is very slow, using preemptive threads: 
             $a looong sp <:xmllist< looong2 >> ()$<br/>
       Catching errors:
             $a catch sp <:xmllist< catch >> 22$ (change the value in the URL)<br/>
       Redirection:
             $a redir sp <:xmllist< redir >> 11$<br/>
       Cookies:
             $a cookies sp <:xmllist< cookies >> ()$<br/>
       Disposable coservices:
             $a disposable sp <:xmllist< disposable >> ()$<br/>
       The following URL send either a statically checked page, or a text page:
             $a sendany sp <:xmllist< sendany >> "valid"$<br/>
       </p>
       </body>
     </html> >>)


(* *zap*)
