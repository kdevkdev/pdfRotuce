$( document ).ready(function() {

         var $$ = $;
         var headings = $$('.section');
         //loop through these and add elements to toc taking into account hierarchy //
         
         
         // code generated with copilot 
        var rootUL = $$("<ul/>").attr("class", "nav");
        var listStack = [rootUL];
        var navLevel = 1;

         headings.each(function() {
            var a = $$(this); 
            
            var classes = a.attr("class").split(/\s+/);

            var currentLevel = classes.find(( el  ) => { return(el.startsWith("level"))});
            
            if(currentLevel){
              currentLevel = currentLevel.match(/\d+/);
            }
            
            
            if (currentLevel > navLevel) {
              for (var d = navLevel; d < currentLevel; d++) {
                var lastLI = listStack[listStack.length - 1].children("li:last");
                if (!lastLI.length) lastLI = $$("<li/>").appendTo(listStack[listStack.length - 1]);
                var newUL = $$("<ul/>").appendTo(lastLI);
                listStack.push(newUL);
                console.log("going down");
              }
            } else if (currentLevel < navLevel) {
              for (var u = navLevel; u > currentLevel; u--) 
              listStack.pop();
              console.log("going up");
            }
            navLevel= currentLevel;

            
            var href = "#".concat(a.attr("id"));
            
            var number = a.attr("number");
        
            var curHeading = "";
            
            var childNodes = a.children("h1:first-of-type, h2:first-of-type, h3:first-of-type, h4:first-of-type, h5:first-of-type, h6:first-of-type").contents().filter(function(){ 
  return this.nodeType == Node.TEXT_NODE; 
});

            
            if(childNodes.length) curHeading = childNodes[0].nodeValue.trim();
            
            console.log("current_heading: ".concat(curHeading));
            
            var a_id = "toc-".concat(curHeading.replaceAll(" ", "-").toLowerCase());
            
            // generate list
            link = $$("<a>").attr("href", href).attr("id", a_id);
            span = $$("<span/>").text(number).attr("class", "toc-section-number");
            //span.appendTo(link);
            link.append(span).append(" ".concat(curHeading));

            $$("<li/>").append(link).appendTo(listStack[listStack.length - 1]);

            
 
        }) ;
        

         $$("#toc").empty().append(rootUL); 
  
  
    // Fix for dots in level 1 and level 2 titles
    $('body .section.level1').each(function () {
      $(this).attr("id", $(this).attr("id").replace(/\./g, "-"));
    });
    $('body .section.level2').each(function () {
      $(this).attr("id", $(this).attr("id").replace(/\./g, "-"));
    });
    $('#toc ul li a').each(function () {
      $(this).attr("href", $(this).attr("href").replace(/\./g, "-"));
    });

    // Shift nav in mobile when clicking the menu.
    $(document).on('click', "[data-toggle='wy-nav-top']", function() {
      $("[data-toggle='wy-nav-shift']").toggleClass("shift");
    });
    // Close menu when you click a link.
    $(document).on('click', "#toc ul li a", function() {
      $("[data-toggle='wy-nav-shift']").removeClass("shift");
    });
    // Close menu when you click on main content
    $(document).on('click', "#main, #header", function() {
      $("[data-toggle='wy-nav-shift']").removeClass("shift");
    });

    // Make tables responsive
    $("#main table").wrap("<div class='table-wrapper'></div>");

    // ScrollSpy also requires that we use a Bootstrap nav component.
    $('#toc ul').first().addClass('nav');
    $('body').scrollspy({target: '#toc'});

    // add sticky table headers
    //$('table').stickyTableHeaders();

});
