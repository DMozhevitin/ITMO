<#-- @ftlvariable name="users" type="ru.itmo.wp.model.domain.User[]" -->
<#import "commons.ftlh" as c>

<@c.page>
    <div class="users datatable">
        <div class="caption">User</div>
        <table>
            <thead>
            <tr>
                <th>Id</th>
                <th>Login</th>
                <th class="admin-column">Admin</th>
            </tr>
            </thead>
            <tbody>
            <template>
                <tr>
                    <td class="user_id"></td>
                    <td class="user_login"></td>
                    <td class="user_admin">
                        <p></p>
                        <a href="#"></a>
                    </td>
                </tr>
            </template>
            <tr class="noData">
                <td colspan="3">
                    No data
                </td>
            </tr>
            </tbody>
        </table>
    </div>

    <script>
        $(function () {
            var $noData = $(".users .noData");
            $noData.find("td").text("Loading...");
            var admin = true;

            myAjax({
                action: "checkAdmin"
            }, function (response) {
                if (!response["admin"]) {
                    admin = false;
                    $(".admin-column").hide();
                    $(".user_admin").hide();
                }
            });

            myAjax({
                action: "findAll"
            }, function (response) {
                response["users"].forEach(function (user) {
                    $noData.hide();
                    var $tr = $($(".users template").prop("content")).clone();
                    $tr.find(".user_id").text(user["id"]);
                    $tr.find(".user_login").text(user["login"]);

                    if (!admin) {
                        $tr.find(".user_admin").hide();
                    }

                    var aText = "Enable";
                    if (user["admin"]) {
                        aText = "Disable";
                    }

                    $tr.find(".user_admin p").text(user["admin"]);
                    $tr.find(".user_admin a").text(aText);


                    $tr.find("tr").attr("data-userId", user["id"]);
                    $tr.find("tr a").attr("data-userId", user["id"]);
                    $(".users tbody").append($tr);
                });
                $(".users a").click(function (event) {
                    var $a = $(this);
                    var $tr = $a.parent();
                    myAjax({
                        action: "toggleAdmin",
                        userId: $a.attr("data-userId")
                    }, function (response) {
                        $a.text(toggleAdmin[$a.text()]);
                        var $p = $tr.find("p");
                        var newTxt = ($p.text() === "true");
                        $p.text(!newTxt);
                    });
                });
            });
        })
    </script>
</@c.page>
