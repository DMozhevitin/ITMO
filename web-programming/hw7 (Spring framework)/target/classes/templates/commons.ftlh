<#-- @ftlvariable name="user" type="ru.itmo.wp.domain.User" -->
<#-- @ftlvariable name="message" type="java.lang.String" -->
<#import "/spring.ftl" as spring/>
<#macro page>
    <!DOCTYPE html>
    <html lang="en">
    <head>
        <meta charset="UTF-8">
        <title>Codeforces</title>
        <link rel="stylesheet" type="text/css" href="<@spring.url '/css/normalize.css'/>">
        <link rel="stylesheet" type="text/css" href="<@spring.url '/css/style.css'/>">
        <link rel="stylesheet" type="text/css" href="<@spring.url '/css/form.css'/>">
        <link rel="stylesheet" type="text/css" href="<@spring.url '/css/datatable.css'/>">
        <script src="<@spring.url '/js/jquery-3.4.1.min.js'/>"></script>
        <script src="<@spring.url '/js/notify.js'/>"></script>
    </head>
    <body>
    <header>
        <a href="/"><img src="<@spring.url '/img/logo.png'/>" alt="Codeforces" title="Codeforces"/></a>
        <div class="languages">
            <a href="#"><img src="<@spring.url '/img/gb.png'/>" alt="In English" title="In English"/></a>
            <a href="#"><img src="<@spring.url '/img/ru.png'/>" alt="In Russian" title="In Russian"/></a>
        </div>
        <div class="enter-or-register-box">
            <#if user??>
                ${user.login}
                |
                <a href="/logout">Logout</a>
            <#else>
                <a href="/enter">Enter</a>
                |
                <a href="/register">Register</a>
            </#if>
        </div>
        <nav>
            <ul>
                <li><a href="/">Home</a></li>
                <li><a href="/users/all">Users</a></li>
                <#if user??>
                    <li><a href="/addNotice">Add Notice</a></li>
                </#if>
            </ul>
        </nav>
    </header>
    <div class="middle">
        <aside>
            <#list notices as notice>
                <section>
                    <div class="header">
                        Notice ${notice.id}
                    </div>
                    <div class="body">
                        <#if (notice.content?length > 27)>
                            ${notice.content?substring(0, 27)}...
                        <#else>
                            ${notice.content}
                        </#if>
                    </div>
                    <div class="footer">
                        <a href="#">View all</a>
                    </div>
                </section>
            </#list>
        </aside>
        <main>
            <#nested/>
        </main>
    </div>
    <footer>
        <a href="/">Codeforces</a> &copy; 2019 by Mike Mirzayanov
    </footer>
    </body>
    <script>
        $(function () {
            <#if message??>
            $.notify("${message?js_string}", {className: "success", position: "bottom right"});
            </#if>
        })
    </script>
    </html>
</#macro>

<#macro error field>
    <@spring.bind field/>
    <#if spring.status.errorMessage??>
        <div class="error">${spring.status.errorMessage}</div>
    </#if>
</#macro>

<#macro userlink user>
    <a class="userlink${user.id}" href="/user/${user.id}">${user.login}</a>
</#macro>

