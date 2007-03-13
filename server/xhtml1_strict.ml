(** This interface corresponds to the XHTML 1.0 Strict DTD. *)

type events = {{  {   onkeyup=?String 
		      onkeydown=?String 
		      onkeypress=?String 
		      onmouseout=?String 
		      onmousemove=?String 
		      onmouseover=?String 
		      onmouseup=?String 
		      onmousedown=?String 
		      ondblclick=?String 
		      onclick=?String } }}

type valign = {{ "top" | "middle" | "bottom" | "baseline" }}
type align = {{ "left" | "center" | "right" | "justify" | "char" }}
type scope = {{ "row" | "col" | "rowgroup" | "colgroup" }}
type shape = {{ "rect" | "circle" | "poly" | "default" }}

type id = {{ { id=?String } }}
type coreattrs = {{ { title=?String style=?String class=?String } ++ id }}
type i18n = {{ { dir=?"ltr"|"rtl" xml:lang=?String lang=?String } }}

type focus = {{ {   onblur=?String 
		    onfocus=?String 
		    tabindex=?String 
		    accesskey=?String } }}

type attrs = {{ coreattrs ++ i18n ++ events }}

type align_attrs = {{ attrs ++ { valign =? valign align =? align char =? String  charoff =? String } }}

(* Text elements *)

type special_pre = {{ br | span | bdo | _map }}
and special = {{ special_pre | _object | img }}
and fontstyle = {{ tt | i | b | big | small }}
and phrase = {{ em | strong | dfn | code | q 
	      | samp | kbd | var | cite | abbr | acronym | sub | sup }}
and inline_forms = {{ input | select | textarea | label | button }}
and misc_inline = {{ ins | del | script }}
and misc = {{ noscript | misc_inline }}
and inline = {{ a | special | fontstyle | phrase | inline_forms }}
and inlines = {{ [ (Char | inline | misc_inline)* ] }}

(* Block level elements *)

and heading = {{ h1 | h2 | h3 | h4 | h5 | h6 }}
and lists = {{ ul | ol | dl }}
and blocktext = {{ pre | hr | blockquote | address }}
and block =  {{ p | heading | _div | lists | blocktext | fieldset | table }}
and blocks = {{ [ (block | form | misc)* ] }}
and flows = {{ [ (Char | block | form | inline | misc)* ] }}

(* Content models for exclusions *)


(* Document structure *)

and html = {{ <html (i18n ++ id)>[ head body ] }}

and head_misc = {{ script|style|meta|link|_object }}
and head = {{ <head (i18n ++ id ++ { profile=?String })>[
		head_misc* title head_misc* base? head_misc*
	      | head_misc* base head_misc* title head_misc* ] }}

and title = {{ <title (i18n ++ id)>[ PCDATA ] }}
and base = {{ <base (id ++ { href=String })>[  ] }}
and meta = {{ <meta (i18n ++ id ++ { scheme=?String content=String name=?String http-equiv=?String })>
		[ ] }}

and link_attrs =
    {{ attrs ++ { media=?String rev=?String rel=?String type=?String 
                  hreflang=?String href=?String charset=?String } }}

and link = {{ <link (link_attrs)>[ ] }}

and style = {{ <style (i18n ++ id ++ { title=?String media=?String type=String })>[ PCDATA ] }}

and script_attrs =
   {{ id ++ { defer=?"defer" src=?String type=String charset=?String } }}

and script = {{ <script (script_attrs)>[PCDATA] }}

and noscript = {{ <noscript (attrs)>blocks }}


(* Document body *)

and body = {{ <body (attrs ++ { onload=?String onunload=?String })>blocks }}
and _div = {{ <div (attrs)>flows }}

(* Paragraphs *)

and p = {{ <p (attrs)>inlines }}

(* Headings *)

and h1 = {{ <h1 (attrs)>inlines }}
and h2 = {{ <h2 (attrs)>inlines }}
and h3 = {{ <h3 (attrs)>inlines }}
and h4 = {{ <h4 (attrs)>inlines }}
and h5 = {{ <h5 (attrs)>inlines }}
and h6 = {{ <h6 (attrs)>inlines }}

(* Lists *)

and ul = {{ <ul (attrs)>[ li+ ] }}
and ol = {{ <ol (attrs)>[ li+ ] }}
and li = {{ <li (attrs)>flows }}
and dl = {{ <dl (attrs)>[ (dt|dd)+ ] }}
and dt = {{ <dt (attrs)>inlines }}
and dd = {{ <dd (attrs)>flows }}

(* Address *)

and address = {{ <address (attrs)>inlines }}

(* Horizontal rule *)

and hr = {{ <hr (attrs)>[] }}

(* Preformatted Text *)

and pre = {{ <pre (attrs)>[ (Char | a | fontstyle | phrase | special_pre | misc_inline | inline_forms)* ] }}

(* Block-like quotes *)

and blockquote = {{ <blockquote (attrs ++ { cite =? String })>blocks }}

(* Inserted/Deleted Text *)

and ins = {{ <ins (attrs ++ { cite =? String datetime =? String })>flows }}
and del = {{ <del (attrs ++ { cite =? String datetime =? String })>flows }}

(* The Anchor Element *)

and a_attrs =
    {{ attrs ++ focus ++ { 
		 charset =? String
		 type =? String
		 name =? String
		 href =? String
		 hreflang =? String
		 rel =? String
		 rev =? String
		 shape =? shape
		 coords =? String} }}

and a_content =
     {{ Char | special | fontstyle | phrase | inline_forms | misc_inline
      }}

and a = {{ <a (a_attrs)> [ a_content* ] }}

(* Inline elements *)

and span = {{ <span (attrs)>inlines }}
and bdo = {{ <bdo (attrs & { dir=Any .. })>inlines }}
and br = {{ <br (coreattrs)>[] }}

and em = {{ <em (attrs)>inlines }}
and strong = {{ <strong (attrs)>inlines }}
and dfn = {{ <dfn (attrs)>inlines }}
and code = {{ <code (attrs)>inlines }}
and samp = {{ <samp (attrs)>inlines }}
and kbd = {{ <kbd (attrs)>inlines }}
and var = {{ <var (attrs)>inlines }}
and cite = {{ <cite (attrs)>inlines }}
and abbr = {{ <abbr (attrs)>inlines }}
and acronym = {{ <acronym (attrs)>inlines }}
and q = {{ <q (attrs ++ { cite =? String })>inlines }}
and sub = {{ <sub (attrs)>inlines }}
and sup = {{ <sup (attrs)>inlines }}
and tt = {{ <tt (attrs)>inlines }}
and i = {{ <i (attrs)>inlines }}
and b = {{ <b (attrs)>inlines }}
and big = {{ <big (attrs)>inlines }}
and small = {{ <small (attrs)>inlines }}


(* Object *)

and _object = {{ <object (attrs ++ 
			    { tabindex=?String name=?String 
				usemap=?String width=?String 
				height=?String standby=?String 
				archive=?String codetype=?String 
			        type=?String data=?String codebase=?String 
				classid=?String declare=?"declare" })>
		   [ (Char|param|block|form|inline|misc)* ] }}

and param = {{ <param type=?String valuetype=?"data"|"ref"|"object"
			  value=?String name=?String id=?String>[ ] }}


(* Images *)

and img = {{ <img (attrs ++ { ismap=?"ismap" usemap=?String width=?String 
		       height=?String longdesc=?String alt=String src=String })>[] }}


(* Client-side image maps *)

and _map = {{ <map ((attrs ++ { name=?String }) & { id = Any .. })>[
		(block | form | misc)+ | area+ ] }}

and area = {{ <area (attrs ++ focus ++ { alt=String nohref=?"nohref" 
			 href=?String coords=?String shape=?shape })>[ ] }}


(* Forms *)

and form_attrs = 
  {{ attrs ++ { accept-charset=?String accept=?String 
		onreset=?String onsubmit=?String enctype=?String 
		method=?"get"|"post" action=String} }}

and form_content =
    {{ block|misc }}

and form = {{ <form (form_attrs)> [ form_content* ] }}


and label = {{ <label (attrs ++ { for=?String accesskey=?String onfocus=?String onblur=?String })>
		 inlines }}

and input_type_values =
  {{ "text"|"password"|"checkbox"|"radio"|"submit"
     |"reset"|"file"|"hidden"|"image"|"button" }}

and input_attrs =
    {{ attrs ++ focus ++ 
         {accept=?String onchange=?String 
          onselect=?String usemap=?String alt=?String src=?String 
	  maxlength=?String size=?String readonly=?"readonly"
	  disabled=?"disabled" checked=?"checked" value=?String 
	  name=?String 
	  type=?input_type_values } }}

and input = {{ <input (input_attrs)>[] }}

and select_attrs =
              {{ attrs ++ {onchange=?String 
                             onblur=?String 
                             onfocus=?String 
			     tabindex=?String
                             disabled=?"disabled" 
                             multiple=?"multiple"
			     size=?String
                             name=?String} }}

and select = {{ <select (select_attrs)>[ (optgroup|option)+ ]}}

and optgroup = {{ <optgroup (attrs ++ { disabled=?"disabled" label=String })>[ option+ ] }}
and option = {{ <option (attrs ++ { selected=?"selected" disabled=?"disabled" 
			            label=?String value=?String })>[ PCDATA ] }}

and textarea_attrs =
    {{ attrs ++ focus ++ 
	 { onchange=?String
             onselect=?String 
	     readonly=?"readonly" 
             disabled=?"disabled" 
             cols=String
             rows=String 
	     name=?String } }}

and textarea = {{ <textarea (textarea_attrs)>[ PCDATA ] }}

and fieldset = {{ <fieldset (attrs)>[ (Char|legend|block|form|inline|misc)* ] }}
and legend = {{ <legend (attrs ++ { accesskey=?String })>inlines }}

and button = {{ <button (attrs ++ focus ++ 
			   { name=?String value=?String 
			     type=?"button"|"submit"|"reset" disabled=?"disabled" })>[
			     (Char | p | heading | _div | lists | blocktext | table | special 
			     | fontstyle | phrase | misc)* ] }}


(* Tables *)


and table = {{ <table (attrs ++ {  cellpadding=?String cellspacing=?String 
			       rules=?"none"|"groups"|"rows"|"cols"|"all" 
			       frame=?"void"|"above"|"below"|"hsides"
		                     |"lhs"|"rhs"|"vsides"|"box"|"border" 
			       border=?String width=?String summary=?String })>[
                ( caption? (col* | colgroup* ) thead? tfoot? (tbody+ | tr+) )
                ]}}

and caption = {{ <caption (attrs)>inlines }}

and thead = {{ <thead (align_attrs)>[ tr+ ] }}
and tfoot = {{ <tfoot (align_attrs)>[ tr+ ] }}
and tbody = {{ <tbody (align_attrs)>[ tr+ ] }}
  	
and colgroup = {{ <colgroup (align_attrs ++ { span =? String width =? String})>
		    [ col* ] }}
and col = {{ <col (align_attrs ++ { span=?String width=?String }) >[] }}
and tr = {{ <tr (align_attrs)>[ (th|td)+ ] }}
and th = {{ <th (align_attrs ++ { colspan=?String rowspan=?String 
		            scope=?scope headers=?String axis=?String abbr=?String })>flows }}
and td = {{ <td (align_attrs ++ { colspan=?String rowspan=?String 
		            scope=?scope headers=?String axis=?String abbr=?String })>flows }}
