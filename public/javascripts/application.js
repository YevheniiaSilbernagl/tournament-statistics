jQuery(window).ready(function () {
    $.ajaxSetup({cache: true});
    refresh_links();

    $(document).on('click', "#generateResources", function () {
        generate_resources();
    });

    $(document).click(function (e) {
        if (e.target.tagName.toLowerCase() !== 'input') {
            apply_name();
        }
    });

    $(document).on('keyup', ".deck-name-edit", function (e) {
        if (e.keyCode === 13) {
            apply_name();
        }
    });

    $(document).on('keyup', "input", function (e) {
        apply_score();
    });

    $(document).on('keyup', "#casters-list", function (e) {
        $('#generate-casters-list').attr("href", "/casters?list=" + encodeURIComponent($("#casters-list").val()));
    });


    $(document).on('click', "#generate-stats", function (e) {
        $("#info").each(function () {
            this.remove();
        });
        var players = encodeURIComponent($("#players").val());

        $.get("/players/stats?players=" + players, function (response) {
            var block = "<table id='info' class=\"table table-striped\">" +
                "<tr><th>Player</th><th>This Year</th><th>Career</th></tr>";
            $.each(response.players, function (player) {
                block += "<tr>";
                block += "<td>" + player + "</td>";

                $.each(this, function (tournament_type, stats) {
                    block += "<td><table class=\"table-bordered\">";
                    $.each(stats, function (name, value) {
                        block += "<tr><td>" + name + "</td><td>" + value + "</td></tr>";
                    });
                    block += "</table></td>";
                });
                block += "</tr>";
            });
            block += "</table>";
            $(".container").append(block);
        });
    });

    $(document).on('click', 'input[name=maincam]', function (e) {
        refresh_generate_side_panel_link();
    });

    $(document).on('click', '#check-in', function (e) {
        $(".checkins").each(function () {
            this.remove();
        });
        var block = "<table class='checkins'>";
        var players = $('#ids').val().split("\n");
        players.forEach(function (player) {
            block += "<tr><td class='id' id='" + player + "'>" + $("#info-" + player).text() + "</td></tr>";
        });
        block += "</table>";
        $(".container").append(block);
        var tournament_id = $('.info').attr('id');
        var auth = $("#token").val();
        if (!auth.startsWith("Bearer")) {
            auth = "Bearer " + auth;
        }
        players.forEach(function (id) {
            $.ajax({
                type: "POST",
                url: "https://api.battlefy.com/tournaments/" + tournament_id + "/teams/" + id + "/check-in",
                headers: {
                    "Authorization": auth
                },
                success: function () {
                    $("#" + id).append('<span class="badge badge-pill badge-success"> + </span>');
                },
                error: function () {
                    $("#" + id).append('<span class="badge badge-pill badge-danger"> - </span>');
                }
            });
        })
    });

    $(document).on('click', ".opponent-link", function (e) {
        var currentText = $(e.currentTarget).text();
        $("#opponents").each(function () {
            this.remove();
        });
        $.get("/opponents/info?opponents=" + currentText, function (response) {
            var generated = "";
            response.opponents.forEach(function (opponent) {
                if (opponent.deck != null) {
                    generated += '                    <td>\n' +
                        '                        <div class="deck">\n' +
                        '                           <div class="player-name">\n' +
                        '                               <p class="p-header">\n' +
                        '                                   <a target="_blank" href="/player?playerName=' + opponent.name + '">' + opponent.name + '</a>\n' +
                        '                               </p>\n' +
                        '                               <label><input class="score" type="text" placeholder="' + opponent.name + '\'s score" value="0"></label>' +
                        '                               <a link="' + opponent.deck.url + '" href="/download/tourney-left?link=' + opponent.deck.url + '&name=' + opponent.deck.name + '&player=' + opponent.name + '" class="badge badge-secondary generate-left noprint" download>Main(left)</a>\n' +
                        '                               <a link="' + opponent.deck.url + '" href="/download/tourney-right?link=' + opponent.deck.url + '&name=' + opponent.deck.name + '&player=' + opponent.name + '" class="badge badge-dark generate-right noprint" download>Handcam(right)</a>\n' +
                        '                               <input type="radio" name="maincam"> Main cam' +
                        '                           </div>\n' +
                        '                           <div><p class="p-header deck-name">' + opponent.deck.name + '</p></div>\n' +
                        '                           <textarea rows="20" cols="50">' + opponent.deck.list + '</textarea>\n' +
                        '                        </div>\n' +
                        '                    </td>\n';
                }
            });
            $(".container").append('<div id="opponents">\n' +
                '            <table class="table-bordered">\n' +
                '                <tr>\n' + generated +
                '                </tr>\n' +
                '            </table>\n' +
                '            <a id="generate-panel" class="btn btn-primary" href="#" download>Generate side panel</a>\n' +
                '        </div>');
            refresh_generate_side_panel_link();
        });
    });

    $(document).on('dblclick', ".deck-name", function (e) {
        apply_name();
        var currentText = $(e.currentTarget).text();
        var parent = $(e.currentTarget).parent();
        $(e.currentTarget).remove();
        $(parent).append('<input class="deck-name-edit" type="text" value="' + currentText + '">');
        $('.deck-name-edit').focus()
    });

    function apply_name() {
        var editField = $('.deck-name-edit');
        if (editField.length > 0) {
            var currentText = editField.val();
            var parent = $(editField).parent();
            var deckDiv = $(parent).parent();
            $(editField).remove();
            $(parent).append('<p class="p-header deck-name">' + currentText + '</p>');
            refresh_links();
        }
    }

    function apply_score() {
        refresh_generate_side_panel_link()
    }

    function refresh_generate_side_panel_link() {
        var players = $(".deck");
        var generatedUrl = "/download/streaming/panel?";
        var opponentId = 1;
        players.each(function () {
            var playerDiv = this;
            if (opponentId > 1) generatedUrl += "&";
            generatedUrl += "player" + opponentId + "Name=" + encodeURIComponent($(playerDiv).find(".p-header a").text())
                + "&player" + opponentId + "Score=" + encodeURIComponent($(playerDiv).find("input").val()) +
                "&player" + opponentId + "DeckName=" + encodeURIComponent($(playerDiv).find(".deck-name").text());
            var mainCamPlayer = $('input[name=maincam]:checked').parent().find('p').find('a').text();
            if (mainCamPlayer !== "") generatedUrl += "&mainCam=" + mainCamPlayer;
            opponentId += 1;
        });
        $('#generate-panel').attr("href", generatedUrl);
    }

    function refresh_links() {
        var decks = $(".deck");
        decks.each(function () {
            var deckDiv = this;
            var currentText = encodeURIComponent($(deckDiv).find('.deck-name').text());
            var deckLink = encodeURIComponent($(deckDiv).find('.generate-left').attr("link"));
            var playerName = encodeURIComponent($(deckDiv).find('.player-name').find("p").text());
            $(deckDiv).find('.generate-left').attr("href", "/download/tourney-left?link=" + deckLink + "&name=" + currentText + "&player=" + playerName);
            $(deckDiv).find('.generate-right').attr("href", "/download/tourney-right?link=" + deckLink + "&name=" + currentText + "&player=" + playerName);
        });
        refresh_generate_side_panel_link();
    }

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
            dataType: "json",
            success: function (msg) {
                $("#generatedResources").remove();
                $(".container").append('<div id="generatedResources" class="row">' +
                    '<div class="col-sm-6"><img src="/assets/images/' + msg.left + '" style="width:100%"></div>' +
                    '<div class="col-sm-6"><img src="/assets/images/' + msg.right + '" style="width:100%"></div>' +
                    '</div>');
            }
        });
    }

    $(document).ready(
        function () {
            var eternalFormat = new RegExp('.*\\+\\d{4}');
            var discordFormat = new RegExp('.*\\#\\d{4}');
            var player = $(".player");
            var decks = player.find(".deckLinkBlock");
            var eternalNames = player.find(".eternalName");
            var discordNames = player.find(".discordName");
            decks.each(function () {
                var deck = this;
                var url = $(deck).find(".deckLink").text();
                $.get("/validateDeck?url=" + url, function (response) {
                    //redraw progress
                    if (response.valid === true) {
                        $(deck).addClass("btn-success").removeClass("btn-light")
                    } else {
                        // $(deck).addClass("btn-danger").removeClass("btn-light");
                        response.messages.forEach(function (message) {
                            $(deck).append('<span class="badge badge-pill badge-danger">' + message + '</span>');
                            var errorMessage = '<div style="color: darkred">Hello<br>' +
                                'Your Decklist for this weekends ETS is not marked as Tournament Deck<br>' +
                                'Please update your decklist in Eternal Warcry.<br>' +
                                'If you have any issues please let me know<br>' +
                                'Quick link to your list: <code><</code>' + url + '<code>></code></div>' +
                                '<a href="/message?player=??&message="' + encodeURIComponent('Hello\n' +
                                    'Your Decklist for this weekends ETS is not marked as Tournament Deck\n' +
                                    'Please update your decklist in Eternal Warcry.\n' +
                                    'If you have any issues please let me know\n' +
                                    'Quick link to your list: <' + url + '>') + '</a>';
                            $(deck).append(errorMessage);
                        });
                    }
                });
            });
            eternalNames.each(function () {
                var name = $(this).text().trim();
                if (!eternalFormat.test(name)) {
                    $(this).append('<span class="badge badge-pill badge-danger">Not a valid eternal name</span>')
                }
            });
            discordNames.each(function () {
                var name = $(this).text().trim();
                if (!discordFormat.test(name)) {
                    $(this).append('<span class="badge badge-pill badge-danger">Not a valid discord name</span>')
                }
            });
        }
    )
});