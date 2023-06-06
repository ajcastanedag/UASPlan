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
window.addEventListener('DOMContentLoaded', () => {
  const checklists = document.querySelectorAll('.checklist');

  checklists.forEach(checklist => {
    const checkboxes = checklist.querySelectorAll('input[type="checkbox"]');
    const title = checklist.querySelector('h2');

    checkboxes.forEach(checkbox => {
      checkbox.addEventListener('change', () => {
        const allChecked = [...checkboxes].every(checkbox => checkbox.checked);
        if (allChecked) {
          title.classList.add('completed');
        } else {
          title.classList.remove('completed');
        }
      });
    });
  });
});
// --------------------------------------------------------------------
