<#import "commons.ftlh" as c>

<@c.page>
    <div class="userArticles datatable">
        <div class="caption">My articles</div>
        <table>
            <thead>
            <tr>
                <th>Id</th>
                <th>Title</th>
                <th>Hide</th>
            </tr>
            </thead>
            <tbody>
            <#list userArticles as article>
                <tr class="article-${article.id}">
                    <td>${article.id}</td>
                    <td>
                        <#if (article.title?length > 60)>
                            ${article.title?substring(0, 60)}...
                        <#else>
                            ${article.title}
                        </#if>
                    </td>
                    <td class="hideOrShow">
                        <#if article.hidden>
                            <#assign val = "Show">
                        <#else>
                            <#assign val = "Hide">
                        </#if>
                        <input class="button" type="submit" id="${article.id}" value=${val}>
                    </td>
                </tr>
            </#list>
            </tbody>
        </table>
    </div>

    <script>
        $(function () {
            $(".userArticles .button").click(
                function (event) {
                    var $button = $(this);
                    $button.attr("disabled", true);
                    myAjax({
                        action: "toggleHidden",
                        articleId: $button.attr("id"),
                        value: $button.attr("value")
                },

                    function (response) {
                        $button.attr("value", toggleHidden[$button.attr("value")]);
                        $button.attr("disabled", false);
                    }

                )
                    ;
                }
            )
        })
    </script>
</@c.page>