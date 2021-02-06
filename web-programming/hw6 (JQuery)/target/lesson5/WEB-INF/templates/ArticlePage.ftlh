<#import "commons.ftlh" as c>

<@c.page>
    <div class="article form-box">
        <div class="header">Create article</div>
        <div class="body">
            <form method="post" action="">
                <input type="hidden" name="action" value="addArticle"/>
                <div class="field">
                    <div class="name">
                        <label for="title">Title</label>
                    </div>
                    <div class="value">
                        <input id="title" name="title" value="" maxlength="255" required/>
                    </div>
                </div>

                <div class="field">
                    <div class="name">
                        <label for="text">Text</label>
                    </div>
                    <div class="value">
                        <textarea id="text" name="text" required maxlength="1000"></textarea>
                    </div>
                </div>

                <div class="button-field">
                    <input type="submit" value="Create">
                </div>
            </form>
        </div>
    </div>
    <script>
        $(function () {
            $(".article form").submit(function () {
                var title = $(this).find("input[name='title']").val();
                var text = $(this).find("textarea[name='text']").val();

                myAjax({
                        action: "addArticle",
                        title: title,
                        text: text
                    },
                    function (response) {
                    };)

                return false;
            });
        })
    </script>
</@c.page>