jQuery(window).ready(function () {
    $.ajaxSetup({cache: true});
    refresh_links();
    $("#seriesPointsTable").fancyTable({
        sortColumn: 4,
        sortable: true,
        pagination: true,
        perPage: 15,
        searchable: true,
        globalSearch: false
    });

    $("#invitationalPointsTable").fancyTable({
        sortColumn: 4,
        sortable: true,
        pagination: true,
        perPage: 15,
        searchable: true,
        globalSearch: false
    });

    $("#communityChampionshipPointsTable").fancyTable({
        sortColumn: 4,
        sortable: true,
        pagination: true,
        perPage: 15,
        searchable: true,
        globalSearch: false
    });

    $("#players").fancyTable({
        sortColumn: 0,
        sortable: true,
        pagination: true,
        perPage: 15,
        searchable: true,
        globalSearch: false
    });

    $("#ecqPlayersTable").fancyTable({
        sortColumn: 0,
        sortable: true,
        pagination: true,
        perPage: 65,
        searchable: true,
        globalSearch: false
    });

    $("#usersTable").fancyTable({
        sortColumn: 0,
        sortable: true,
        pagination: true,
        perPage: 20,
        searchable: true,
        globalSearch: false
    });

    $(document).on('click', "#generateResources", function () {
        generate_resources();
    });

    $(document).click(function (e) {
        var tagName = e.target.tagName.toLowerCase();
        if (tagName !== 'input' && tagName !== 'select') {
            apply_name();
            apply_role();
        }
    });

    $(document).on('keyup', ".deck-name-edit", function (e) {
        if (e.keyCode === 13) {
            apply_name();
        }
    });

    $(document).on('keyup', ".user-role-edit", function (e) {
        if (e.keyCode === 13) {
            apply_role();
        }
    });

    $(document).on('keyup', "input", function (e) {
        apply_score();
    });

    $(document).on('keyup', "#casters-list", function (e) {
        var list = encodeURIComponent($("#casters-list").val());
        $('#generate-casters-list').attr("href", "/casters?list=" + list);
        $('#generate-casters-list-ecq').attr("href", "/casters/ecq?list=" + list);
    });


    $(document).on('keyup', "#scene-name", function (e) {
        update_card_list_url()
    });

    $(document).on('keyup', "#cards-list", function (e) {
        update_card_list_url();
    });

    function update_card_list_url() {
        var lines = $("#cards-list").val().split("\n");
        var url = "/download/list/cards?header=" + encodeURIComponent($("#scene-name").val());
        $.each(lines, function (l) {
            var line = this.split("(")[0].trim();
            var cardName = line.substring(line.indexOf(" "), line.length).trim();
            url += "&cards=" + encodeURIComponent(cardName);
        });
        $("#generate-cards-list").attr("href", url);
    }

    $(document).on('click', "#add-ecq-player", function (e) {
        var playerName = $('#playerName').val();
        var deckName = $('#deckName').val();
        var deckList = $('#deckList').val();
        var status = $("#status");
        status.text("");
        $.ajax({
            type: "POST",
            url: "/add/ecq/player?playerName=" + encodeURIComponent(playerName) + "&deckName=" + encodeURIComponent(deckName),
            data: deckList,
            contentType: "text/plain; charset=utf-8",
            success: function (r) {
                status.addClass("alert-success");
                status.text(r)
            },
            error: function (r) {
                status.addClass("alert-danger");
                status.text(r.responseText)
            }
        });
    });

    $(document).on('click', "#create-user", function (e) {
        var login = $("#login").val();
        var password = $("#password").val();
        var role = $('#role').val();
        var status = $("#status");
        status.text("");
        if (login && password && role) {
            var request = JSON.stringify({
                login: login,
                role: role,
                password: password
            });
            $.ajax({
                type: "POST",
                url: "/create/user",
                data: request,
                contentType: "application/json; charset=utf-8",
                success: function (r) {
                    status.addClass("alert-success");
                    status.text(r)
                },
                error: function (r) {
                    status.addClass("alert-danger");
                    status.text(r.responseText)
                }
            });
        } else {
            status.addClass("alert-danger");
            status.text("Login/password/role cannot be empty")
        }
    });

    $(document).on('click', "#import-tournament", function (e) {
        var status = $("#status");
        var season = $("#season").val();
        var url = "/import/tournament?battlefyUuid=" + $("#tournament-id").val() + "&season=" + season + "&tournamentType=" + $("#tournament-type").val();
        status.text("");
        status.removeClass("alert-danger");
        status.removeClass("alert-success");
        if (season < 1 || season > 4) {
            status.addClass("alert-danger");
            status.text("Season should have value 1 to 4")
        } else {
            $.ajax({
                type: "POST",
                url: url,
                success: function (r) {
                    status.addClass("alert-success");
                    status.text(r)
                },
                error: function (r) {
                    status.addClass("alert-danger");
                    status.text(r.responseText)
                }
            });
        }
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
            $(".content").append(block);
        });
    });

    $(document).on('click', 'input[name=maincam]', function (e) {
        var currentTargetName = "." + e.currentTarget.className;
        $(currentTargetName + ".generate-right").css('visibility', 'hidden');
        $(currentTargetName + ".generate-left").css('visibility', 'visible');
        $(".generate-left").not(currentTargetName).css('visibility', 'hidden');
        $(".generate-right").not(currentTargetName).css('visibility', 'visible');
        refresh_generate_side_panel_link();
    });

    $(document).on('click', '#check-in', function (e) {
        $(".checkins").each(function () {
            this.remove();
        });
        var block = "<div class='checkins'>";
        var players = $('#ids').val().split(/\s+/);
        players.forEach(function (player) {
            block += "<div class='alert id' role='alert'  id='" + player + "'>" + $("#info-" + player).text() + "</div>";
        });
        block += "</div>";
        $(".content").append(block);
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
                    $("#" + id).addClass("alert-success");
                },
                error: function () {
                    $("#" + id).addClass("alert-danger");
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
                    generated += '                    <td style="padding: 25px">\n' +
                        '                        <div class="deck">\n' +
                        '                           <div class="player-name">\n' +
                        '                               <p class="p-header">\n' +
                        '                                   <a target="_blank" href="/player?playerName=' + encodeURIComponent(opponent.name) + '">' + opponent.name + '</a>\n' +
                        '                               </p>\n' +
                        '                               <label><input class="score" type="text" placeholder="' + opponent.name + '\'s score" value="0"></label>' +
                        '                               <a link="' + opponent.deck.url + '" href="/download/tourney-left?link=' + encodeURIComponent(opponent.deck.url) + '&name=' + encodeURIComponent(opponent.deck.name) + '&player=' + encodeURIComponent(opponent.name) + '" class="badge badge-secondary generate-left noprint ' + opponent.name.replace("+", "_").replace(" ", "_") + '" download>Main(left)</a>\n' +
                        '                               <a link="' + opponent.deck.url + '" href="/download/tourney-right?link=' + encodeURIComponent(opponent.deck.url) + '&name=' + encodeURIComponent(opponent.deck.name) + '&player=' + encodeURIComponent(opponent.name) + '" class="badge badge-dark generate-right noprint ' + opponent.name.replace("+", "_").replace(" ", "_") + '" download>Handcam(right)</a>\n' +
                        '                               <a link="' + opponent.deck.url + '" href="/download/deck?link=' + encodeURIComponent(opponent.deck.url) + '&name=' + encodeURIComponent(opponent.deck.name) + '&player=' + encodeURIComponent(opponent.name) + '" class="badge badge-dark generate-full-screen noprint ' + opponent.name.replace("+", "_").replace(" ", "_") + '" download>Full screen</a>\n' +
                        '                               <input type="radio" name="maincam" class="' + opponent.name.replace("+", "_").replace(" ", "_") + '"> Main cam' +
                        '                           </div>\n' +
                        '                           <div deck-link="' + opponent.deck.url + '"><p class="p-header deck-name">' + opponent.deck.name + '</p></div>\n' +
                        '                           <pre data-spy="scroll" style="height: 15pc">' + opponent.deck.list + '</pre>\n' +
                        '                        </div>\n' +
                        '                    </td>\n';
                } else {
                    generated += '                    <td style="padding: 25px">\n' +
                        '                        <div class="deck">\n' +
                        '                           <div class="player-name">\n' +
                        '                               <p class="p-header">\n' +
                        '                                   <a target="_blank" href="/player?playerName=' + encodeURIComponent(opponent.name) + '">' + opponent.name + '</a>\n' +
                        '                               </p>\n' +
                        '                           </div>\n' +
                        '                           <pre data-spy="scroll" style="height: 15pc">Deck cannot be parsed</pre>\n' +
                        '                        </div>\n' +
                        '                    </td>\n';
                }
            });
            $(".content").append('<div id="opponents">\n' +
                '            <table class="table-bordered" width="100%">\n' +
                '                <tr>\n' + generated +
                '                </tr>\n' +
                '            </table>\n' +
                '            <a id="generate-panel" class="btn btn-primary btn-lg btn-block" href="#" download>Generate side panel</a>\n' +
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
            var dn = $(deckDiv).find(".deck-name");
            cache_deck_name(dn.text(), dn.parent().attr("deck-link"));
        }
    }

    function apply_role() {
        var editField = $('.user-role-edit');
        if (editField.val()) {
            var newRole = editField.val();
            var parent = $(editField).parent();
            var username = parent.parent().find('.username').text();
            $(editField).remove();
            $(parent).append('<div class="user-role">' + newRole + '</div>');
            save_role(username, newRole);
        }
    }

    function save_role(username, role) {
        $.ajax({
            type: "POST",
            url: "/edit/user/role",
            data: JSON.stringify({
                login: username,
                role: role
            }),
            contentType: "application/json; charset=utf-8",
            dataType: "json"
        });
    }

    $(document).on('click', ".user-delete", function (e) {
        var tr = $(e.currentTarget).parent().parent();
        var username = tr.find('.username').text();
        $.ajax({
            type: "DELETE",
            url: "/user/delete/" + username,
            success: function (msg) {
                tr.remove();
            }
        });
    });

    $(document).on('click', ".player-delete", function (e) {
        var tr = $(e.currentTarget).parent().parent();
        var playername = tr.find('.playername').text();
        $.ajax({
            type: "DELETE",
            url: "/delete/ecq/player/" + playername,
            success: function (msg) {
                tr.remove();
            }
        });
    });

    $(document).on('change', '#playerName1', function (e) {
        update_generate_ecq_side_panel();
        update_player1_deck_link();
        update_player2_deck_link();
    });

    $(document).on('change', '#playerName2', function (e) {
        update_generate_ecq_side_panel();
        update_player1_deck_link();
        update_player2_deck_link();
    });

    $(document).on('keyup', "#score1", function (e) {
        if (e.keyCode !== 13) {
            update_generate_ecq_side_panel();
        }
    });

    $(document).on('keyup', "#score2", function (e) {
        if (e.keyCode !== 13) {
            update_generate_ecq_side_panel();
        }
    });

    function update_player1_deck_link() {
        var playerInfo = $("#playerName1").val();
        if (playerInfo) {
            var player1Name = playerInfo.split(" - ")[0];
            var generatedUrl = "/generate/ecq/deck" +
                "?playerName=" + encodeURIComponent(player1Name) +
                "&side=left";
            $('#player1Deck').attr("href", generatedUrl);
            $('#player1DeckFull').attr("href", "/generate/ecq/deck/full?playerName=" + encodeURIComponent(player1Name));
        }
    }

    function update_player2_deck_link() {
        var playerInfo = $("#playerName2").val();
        if (playerInfo) {
            var player2Name = playerInfo.split(" - ")[0];
            var generatedUrl = "/generate/ecq/deck" +
                "?playerName=" + encodeURIComponent(player2Name) +
                "&side=right";
            $('#player2Deck').attr("href", generatedUrl);
            $('#player2DeckFull').attr("href", "/generate/ecq/deck/full?playerName=" + encodeURIComponent(player2Name));
        }
    }

    function update_generate_ecq_side_panel() {
        var player1Name = $("#playerName1").val();
        var player1Score = $("#score1").val();
        var player2Name = $("#playerName2").val();
        var player2Score = $("#score2").val();
        var generatedUrl = "/generate/ecq/side/panel" +
            "?player1Name=" + encodeURIComponent(player1Name) +
            "&player1Score=" + encodeURIComponent(player1Score) +
            "&player2Name=" + encodeURIComponent(player2Name) +
            "&player2Score=" + encodeURIComponent(player2Score);
        $('#generate-ecq-panel').attr("href", generatedUrl);
    }

    $(document).on('dblclick', ".user-role", function (e) {
        apply_role();
        var currentText = $(e.currentTarget).text();
        var parent = $(e.currentTarget).parent();
        $(e.currentTarget).remove();
        $(parent).append('<select class="user-role-edit">' +
            '<option>ADMIN</option>' +
            '<option>TO</option>' +
            '<option>PRODUCER</option>' +
            '</select>');
        $('.user-role-edit').focus()
    });


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

    function cache_deck_name(deckName, deckLink) {
        $.ajax({
            type: "POST",
            url: "/cache/deck/name",
            data: JSON.stringify({
                link: deckLink,
                name: deckName
            }),
            contentType: "application/json; charset=utf-8",
            dataType: "json"
        });
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
            $(deckDiv).find('.generate-full-screen').attr("href", "/download/deck?link=" + deckLink + "&name=" + currentText + "&player=" + playerName);
        });
        refresh_generate_side_panel_link();
        update_player1_deck_link();
        update_player2_deck_link();
        update_generate_ecq_side_panel();
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
                $(".content").append('<div id="generatedResources" class="row">' +
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
        }
    )
})
;