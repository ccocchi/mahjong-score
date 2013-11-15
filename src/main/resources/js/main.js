$(function() {
  $("#hand_submit").click(function(e) {
    e.preventDefault(); 

    $.post("/hand", $("#content form").serialize(), function(data) {
      $("#response-content").append(data);
    });

    return false;
  });

  $("#reset").click(function(e) {
    e.preventDefault(); 

    $("#hand_input").val("");
    $(".response").remove();   

    return false;
  });
});