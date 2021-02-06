<#import "commons.ftlh" as c>

<@c.page>
    <div class="user-page">
        <#if viewedUser??>
            <div class="user-profile">
                <div class="container-left">
                    <@c.userlink user=viewedUser/>

                    <ul>
                        <li>id: ${viewedUser.id} </li>
                        <li>login: ${viewedUser.login} </li>
                        <li>created: ${viewedUser.creationTime} </li>
                    </ul>
                </div>
            </div>
        <#else>
            <h1> No such user </h1>
        </#if>
    </div>
</@c.page>