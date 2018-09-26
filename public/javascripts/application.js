jQuery(window).ready(function () {
    $.ajaxSetup({cache: true});


    $(document).on('click', "#generateResources", function () {
        generate_resources();
    });

    function generate_resources() {
        $.ajax({
            type: "POST",
            url: "/resources",
            data: JSON.stringify({
                players: $("#matches").val(),
                p1score: $("#currentScorePlayer1").val(),
                p2score: $("#currentScorePlayer2").val()
            }),
            contentType: "application/json; charset=utf-8",
            dataType: "json"
        });
    }
});