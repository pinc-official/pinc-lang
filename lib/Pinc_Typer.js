// Generated by Melange

import * as List from "melange/lib/es6/list.js";
import * as $$Array from "melange/lib/es6/array.js";
import * as Curry from "melange/lib/es6/curry.js";
import * as $$Option from "melange/lib/es6/option.js";
import * as Stdlib from "melange/lib/es6/stdlib.js";
import * as StringMap from "./StringMap.js";
import * as Caml_option from "melange/lib/es6/caml_option.js";

function required(fn, value) {
  var v = Curry._1(fn, value);
  if (v !== undefined) {
    return Caml_option.valFromOption(v);
  } else {
    return Stdlib.failwith("required a value, but got null");
  }
}

function attribute(key, fn, value) {
  return $$Option.bind(Curry._2(StringMap.find_opt, key, value), fn);
}

function string(s) {
  if (typeof s === "number") {
    return ;
  }
  switch (s.TAG | 0) {
    case /* Portal */0 :
        return Stdlib.failwith("expected string, got portal value");
    case /* String */1 :
        return s._0;
    case /* Int */2 :
        return Stdlib.failwith("expected string, got int");
    case /* Float */3 :
        return Stdlib.failwith("expected string, got float");
    case /* Bool */4 :
        return Stdlib.failwith("expected string, got bool");
    case /* Array */5 :
        return Stdlib.failwith("expected string, got array");
    case /* Record */6 :
        return Stdlib.failwith("expected string, got record");
    case /* Function */7 :
        return Stdlib.failwith("expected string, got function definition");
    case /* DefinitionInfo */8 :
        return Stdlib.failwith("expected string, got definition info");
    case /* TagInfo */9 :
        return Stdlib.failwith("expected string, got tag");
    case /* HtmlTemplateNode */10 :
        return Stdlib.failwith("expected string, got HTML template node");
    case /* ComponentTemplateNode */11 :
        return Stdlib.failwith("expected string, got component template node");
    
  }
}

function $$int(i) {
  if (typeof i === "number") {
    return ;
  }
  switch (i.TAG | 0) {
    case /* Portal */0 :
        return Stdlib.failwith("expected int, got portal value");
    case /* String */1 :
        return Stdlib.failwith("expected int, got string");
    case /* Int */2 :
        return i._0;
    case /* Float */3 :
        return Stdlib.failwith("expected int, got float");
    case /* Bool */4 :
        return Stdlib.failwith("expected int, got bool");
    case /* Array */5 :
        return Stdlib.failwith("expected int, got array");
    case /* Record */6 :
        return Stdlib.failwith("expected int, got record");
    case /* Function */7 :
        return Stdlib.failwith("expected int, got function definition");
    case /* DefinitionInfo */8 :
        return Stdlib.failwith("expected int, got definition info");
    case /* TagInfo */9 :
        return Stdlib.failwith("expected int, got tag");
    case /* HtmlTemplateNode */10 :
        return Stdlib.failwith("expected int, got HTML template node");
    case /* ComponentTemplateNode */11 :
        return Stdlib.failwith("expected int, got component template node");
    
  }
}

function $$float(f) {
  if (typeof f === "number") {
    return ;
  }
  switch (f.TAG | 0) {
    case /* Portal */0 :
        return Stdlib.failwith("expected float, got portal value");
    case /* String */1 :
        return Stdlib.failwith("expected float, got string");
    case /* Int */2 :
        return Stdlib.failwith("expected float, got int");
    case /* Float */3 :
        return f._0;
    case /* Bool */4 :
        return Stdlib.failwith("expected float, got bool");
    case /* Array */5 :
        return Stdlib.failwith("expected float, got array");
    case /* Record */6 :
        return Stdlib.failwith("expected float, got record");
    case /* Function */7 :
        return Stdlib.failwith("expected float, got function definition");
    case /* DefinitionInfo */8 :
        return Stdlib.failwith("expected float, got definition info");
    case /* TagInfo */9 :
        return Stdlib.failwith("expected float, got tag");
    case /* HtmlTemplateNode */10 :
        return Stdlib.failwith("expected float, got HTML template node");
    case /* ComponentTemplateNode */11 :
        return Stdlib.failwith("expected float, got component template node");
    
  }
}

function bool(b) {
  if (typeof b === "number") {
    return ;
  }
  switch (b.TAG | 0) {
    case /* Portal */0 :
        return Stdlib.failwith("expected bool, got portal value");
    case /* String */1 :
        return Stdlib.failwith("expected bool, got string");
    case /* Int */2 :
        return Stdlib.failwith("expected bool, got int");
    case /* Float */3 :
        return Stdlib.failwith("expected bool, got float");
    case /* Bool */4 :
        return b._0;
    case /* Array */5 :
        return Stdlib.failwith("expected bool, got array");
    case /* Record */6 :
        return Stdlib.failwith("expected bool, got record");
    case /* Function */7 :
        return Stdlib.failwith("expected bool, got function definition");
    case /* DefinitionInfo */8 :
        return Stdlib.failwith("expected bool, got definition info");
    case /* TagInfo */9 :
        return Stdlib.failwith("expected bool, got tag");
    case /* HtmlTemplateNode */10 :
        return Stdlib.failwith("expected bool, got HTML template node");
    case /* ComponentTemplateNode */11 :
        return Stdlib.failwith("expected bool, got component template node");
    
  }
}

function array(fn, a) {
  if (typeof a === "number") {
    return ;
  }
  switch (a.TAG | 0) {
    case /* Portal */0 :
        return Stdlib.failwith("expected array, got portal value");
    case /* String */1 :
        return Stdlib.failwith("expected array, got string");
    case /* Int */2 :
        return Stdlib.failwith("expected array, got int");
    case /* Float */3 :
        return Stdlib.failwith("expected array, got float");
    case /* Bool */4 :
        return Stdlib.failwith("expected array, bool");
    case /* Array */5 :
        return List.map(fn, $$Array.to_list(a._0));
    case /* Record */6 :
        return Stdlib.failwith("expected array, got record");
    case /* Function */7 :
        return Stdlib.failwith("expected array, got function definition");
    case /* DefinitionInfo */8 :
        return Stdlib.failwith("expected array, got definition info");
    case /* TagInfo */9 :
        return Stdlib.failwith("expected array, got tag");
    case /* HtmlTemplateNode */10 :
        return Stdlib.failwith("expected array, got HTML template node");
    case /* ComponentTemplateNode */11 :
        return Stdlib.failwith("expected array, got component template node");
    
  }
}

function record(r) {
  if (typeof r === "number") {
    return ;
  }
  switch (r.TAG | 0) {
    case /* Portal */0 :
        return Stdlib.failwith("expected record, got portal value");
    case /* String */1 :
        return Stdlib.failwith("expected record, got string");
    case /* Int */2 :
        return Stdlib.failwith("expected record, got int");
    case /* Float */3 :
        return Stdlib.failwith("expected record, got float");
    case /* Bool */4 :
        return Stdlib.failwith("expected record, got bool");
    case /* Array */5 :
        return Stdlib.failwith("expected record, got array");
    case /* Record */6 :
        return Caml_option.some(r._0);
    case /* Function */7 :
        return Stdlib.failwith("expected record, got function definition");
    case /* DefinitionInfo */8 :
        return Stdlib.failwith("expected record, got definition info");
    case /* TagInfo */9 :
        return Stdlib.failwith("expected record, got tag");
    case /* HtmlTemplateNode */10 :
        return Stdlib.failwith("expected record, got HTML template node");
    case /* ComponentTemplateNode */11 :
        return Stdlib.failwith("expected record, got component template node");
    
  }
}

function definition_info($staropt$star, param) {
  var typ = $staropt$star !== undefined ? $staropt$star : "All";
  if (typeof param === "number") {
    return ;
  }
  switch (param.TAG | 0) {
    case /* Portal */0 :
        return Stdlib.failwith("expected definition info, got portal value");
    case /* String */1 :
        return Stdlib.failwith("expected definition info, got string");
    case /* Int */2 :
        return Stdlib.failwith("expected definition info, got int");
    case /* Float */3 :
        return Stdlib.failwith("expected definition info, got float");
    case /* Bool */4 :
        return Stdlib.failwith("expected definition info, got bool");
    case /* Array */5 :
        return Stdlib.failwith("expected definition info, got array");
    case /* Record */6 :
        return Stdlib.failwith("expected definition info, got record");
    case /* Function */7 :
        return Stdlib.failwith("expected definition info, got function definition");
    case /* DefinitionInfo */8 :
        var match = param._0;
        var def_typ = match[1];
        var name = match[0];
        var typ$1;
        var exit = 0;
        var exit$1 = 0;
        var exit$2 = 0;
        var exit$3 = 0;
        if (def_typ !== undefined) {
          if ((typ === "Component" || typ === "All") && def_typ === "Component") {
            typ$1 = "Component";
          } else {
            exit$3 = 4;
          }
        } else {
          typ$1 = Stdlib.failwith("definition \"" + (name + "\" does not exist"));
        }
        if (exit$3 === 4) {
          if (typ === "Page" || typ === "All") {
            if (def_typ === "Page") {
              typ$1 = "Page";
            } else {
              exit$2 = 3;
            }
          } else if (typ === "Component") {
            typ$1 = Stdlib.failwith("expected a component definition");
          } else {
            exit$2 = 3;
          }
        }
        if (exit$2 === 3) {
          if (typ === "Site" || typ === "All") {
            if (def_typ === "Site") {
              typ$1 = "Site";
            } else {
              exit$1 = 2;
            }
          } else if (typ === "Page") {
            typ$1 = Stdlib.failwith("expected a page definition");
          } else {
            exit$1 = 2;
          }
        }
        if (exit$1 === 2) {
          if (typ === "Library" || typ === "All") {
            if (def_typ !== undefined && !(typeof def_typ === "string" || def_typ.NAME !== "Library")) {
              typ$1 = "Library";
            } else {
              exit = 1;
            }
          } else if (typ === "Site") {
            typ$1 = Stdlib.failwith("expected a site definition");
          } else {
            exit = 1;
          }
        }
        if (exit === 1) {
          typ$1 = typ === "Store" || typ === "All" ? (
              def_typ === "Store" ? "Store" : Stdlib.failwith("expected a store definition")
            ) : (
              typ === "Library" ? Stdlib.failwith("expected a library definition") : Stdlib.failwith("expected a store definition")
            );
        }
        return [
                typ$1,
                name,
                match[2] === "NotNegated"
              ];
    case /* TagInfo */9 :
        return Stdlib.failwith("expected definition info, got tag");
    case /* HtmlTemplateNode */10 :
        return Stdlib.failwith("expected definition info, got HTML template node");
    case /* ComponentTemplateNode */11 :
        return Stdlib.failwith("expected definition info, got component template node");
    
  }
}

var Expect = {
  required: required,
  attribute: attribute,
  string: string,
  $$int: $$int,
  $$float: $$float,
  bool: bool,
  array: array,
  record: record,
  definition_info: definition_info
};

export {
  Expect ,
}
/* StringMap Not a pure module */