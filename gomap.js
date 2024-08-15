// When locator icon in datatable is clicked, go to that spot on the map
$(document).on("click", ".go-map", function(e) {
  e.preventDefault();
  $el = $(this);
   var lat = $el.data("lat");
   var long = $el.data("long");
   var cod6 = $el.data("cod6");



  $($("#nav a")[0]).tab("show");
  Shiny.onInputChange("goto", {
     lat: lat,
     lng: long,
    cod6: cod6
  });
});

