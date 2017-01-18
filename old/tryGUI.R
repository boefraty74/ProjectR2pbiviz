library(gWidgets)
options("guiToolkit"="RGtk2")

win <- gwindow("Hello World, ad nauseum", visible=TRUE)
group <- ggroup(horizontal = FALSE, container=win)

obj1 <- gbutton("Hello...",container=group, handler = function(h,...) gmessage("world"))
obj2 <- glabel("Hello...", container =group,handler = NULL)
obj3 <- gcombobox(c("Hello","world","world123","baba"), container=group)
obj23 <- gcombobox(c("Hello","world","world123","baba"), container=group)
obj4 <- gedit("Hello world", container=group)
obj5 <- gtext("Hello world", container=group, font.attr=list(style="bold"))

gbutton("File",handler=function(h,...) print("file"), container=group)
gbutton("Edit",handler=function(h,...) print("edit"), container=group)

group1 <- ggroup(horizontal = T, container=win)

svalue(obj3) <- "world123"
svalue(obj3)

fff = function(obj4){svalue(obj4)<-"aaaa"} 

addHandlerChanged(obj3, handler = fff(obj4), action = NULL) 

enabled(obj23)=F
