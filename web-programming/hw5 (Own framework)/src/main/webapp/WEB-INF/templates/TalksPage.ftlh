<#import "commons.ftlh" as c>

<@c.page>
    <div class="form-box">
        <form method="post" name="sendMessage" action="">
            <input type="hidden" name="action" value="sendMessage"/>
            <div class="field">
                <div class="name">
                    <label for="userMessage">Message</label>
                </div>
                <div class="value">
                    <textarea id="userMessage" name="userMessage" maxlength="1000" required></textarea>
                </div>
            </div>

            <div class="field">
                <div class="name">
                    <label for="target">target</label>
                </div>

                <div class="value">
                    <select name="target" id="target">
                        <#list users! as u>
                            <#if u.id != user.id>
                                <option id="${u.id}" name="${u.id}" value="${u.id}"> ${u.login}</option>
                            </#if>
                        </#list>
                    </select>
                </div>
            </div>

            <#if error??>
                <div class="error">
                    ${error!}
                </div>
            </#if>

            <div class="button-field">
                <input type="submit" value="Send">
            </div>
        </form>
    </div>

    <h2> Received </h2>
    <@c.messageTable received/>

    <h2> Sent </h2>
    <@c.messageTable sent/>
</@c.page>