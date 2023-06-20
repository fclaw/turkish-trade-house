

export const set = function(cookie) { 
    return () => { 
      document.cookie = 
        cookie.getName() + "=" + 
        (cookie.getValue() || "")  + 
        cookie.getExpires() + 
        "; path=" + cookie.getPath();
    }; 
}

export const getIml = nothing => wrap => name => {
  return () => {
    var nameEQ = name + "=";
    var ca = document.cookie.split(';');
    var cookie;
    for(var i=0;i < ca.length;i++) {
        var c = ca[i];
        while (c.charAt(0)==' ') c = c.substring(1,c.length);
        if (c.indexOf(nameEQ) == 0) 
        cookie = c.substring(nameEQ.length,c.length);
    }
    return cookie !== undefined ? wrap(cookie) : nothing; 
  }
}
