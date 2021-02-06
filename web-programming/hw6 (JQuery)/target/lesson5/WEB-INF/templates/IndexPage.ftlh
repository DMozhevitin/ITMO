<#import "commons.ftlh" as c>

<@c.page>
    <div class="articles">
        <ul id="articles-list">
            <template>
                <li class="article_data">
                    <article>
                        <div class="title"></div>
                        <div class="information"></div>
                        <div class="body"></div>
                        <div class="footer">
                            <div class="left">
                                <img src="../../img/voteup.png" title="Vote Up" alt="Vote Up"/>
                                <span class="positive-score">+173</span>
                                <img src="../../img/votedown.png" title="Vote Down" alt="Vote Down"/>
                            </div>
                            <div class="right">
                                <img src="../../img/date_16x16.png" title="Publish Time" alt="Publish Time"/>
                                <span class="days-ago">2 days ago</span>
                                <img src="../../img/comments_16x16.png" title="Comments" alt="Comments"/>
                                <a href="#">68</a>
                            </div>
                        </div>
                    </article>
                </li>
            </template>
            <li class="noData">No data</li>
        </ul>
    </div>

    <script>
        $(function () {
            $("aside section").click(function () {
                $(this).css("background-color", "red");
            });

            var $noData = $(".articles .noData");
            $noData.text("Loading...");

            myAjax({action: "findAll"}, function (response) {
                $noData.hide();
                response["articles"].forEach(function (article) {
                    var $li = $($(".articles template").prop("content")).clone();
                    var title = article["title"];
                    if (title.length > 50) {
                        title = title.substr(0, 50);
                        title += "...";
                    }

                    $li.find(".title").text(title);
                    var text = article["text"];

                    if (text.length > 90) {
                        text = text.substr(0, 90);
                        text += "...";
                    }

                    $li.find(".body").text(text);
                    $li.find(".information").attr("id", article["id"]);

                    myAjax({
                       action: "findUser",
                       userId: article["userId"]
                    }, function(response) {
                        var login = response["foundUser"].login;
                        $($("#" + article["id"])).text("By " + login);
                    });

                    $li.find(".footer .right span").text(article["creationTime"]);
                    $("#articles-list").append($li);
                })
            });
        });
    </script>
</@c.page>
