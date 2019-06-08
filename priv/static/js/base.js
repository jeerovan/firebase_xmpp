//returns xmlhttp object based on browser
function xmlhttpobject(){
        if (window.XMLHttpRequest)
          {// code for IE7+, Firefox, Chrome, Opera, Safari
          xmlhttp=new XMLHttpRequest();
          }
        else
          {// code for IE6, IE5
          xmlhttp=new ActiveXObject("Microsoft.XMLHTTP");
          }
        return xmlhttp;
}
//Shortcut For getElementById
function Z(e) {
        try{
          return document.querySelector("#"+e);
        }
        catch(err){console.log(e);}
}
//GetElementById Inside An Id Element
Element.prototype.Z=function(e){
        return this.querySelector("#"+e);
}
//Shortcut For getElementsByClassName
function Y(e){
        return document.querySelectorAll("."+e);
}
//GetElementByClassName Inside A Class Element
Element.prototype.Y=function(e){
        return this.querySelectorAll("."+e);
}
//Shortcut For getElementsByTagName
function X(e){
        return document.getElementsByTagName(e);
}
//GetElementsByTagName Within A Tag Element
Element.prototype.X=function(e){
        return this.getElementsByTagName(e);
}
