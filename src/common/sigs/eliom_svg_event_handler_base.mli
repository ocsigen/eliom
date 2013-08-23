type event
type mouseEvent
type keyboardEvent

val a_onabort : event -> [> | `OnAbort] attrib
val a_onclick : mouseEvent -> [> | `OnClick] attrib
val a_onmousedown : mouseEvent -> [> | `OnMouseDown] attrib
val a_onmouseup : mouseEvent -> [> | `OnMouseUp] attrib
val a_onmouseover : mouseEvent -> [> | `OnMouseOver] attrib
val a_onmousemove : mouseEvent -> [> | `OnMouseMove] attrib
val a_onmouseout : mouseEvent -> [> | `OnMouseOut] attrib
val a_onscroll : event -> [> | `OnScroll] attrib
val a_onload : event -> [> | `OnLoad] attrib
val a_onresize : event -> [> | `OnResize] attrib
