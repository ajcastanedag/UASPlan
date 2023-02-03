// --------------------------------------------------------------------
// Function to load HTML Documents inside others
function myFunction() {
      var x = document.getElementById("CameraImage");
      if (x.style.display === "none") {
        x.style.display = "block";
      } else {
        x.style.display = "none";
      }
}

// --------------------------------------------------------------------
// Function to load HTML Documents inside others
$(function () {
  var includes = $('[data-include]')
  $.each(includes, function () {
    var file = '1_Protocols/' + $(this).data('include') + '.html'
    $(this).load(file)
  })
})
// --------------------------------------------------------------------
