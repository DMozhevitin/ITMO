<#-- @ftlvariable name="post" type="ru.itmo.tpl.model.Post" -->

<#macro header>
    <header>
        <a href="/"><img src="/img/logo.png" alt="Codeforces" title="Codeforces"/></a>
        <div class="languages">
            <a href="#"><img src="/img/gb.png" alt="In English" title="In English"/></a>
            <a href="#"><img src="/img/ru.png" alt="In Russian" title="In Russian"/></a>
        </div>
        <div class="enter-or-register-box">
            <#if user??>
                <@userlink user=user nameOnly=true />
                |
                <a href="#">Logout</a>
            <#else>
                <a href="#">Enter</a>
                |
                <a href="#">Register</a>
            </#if>
        </div>
        <nav>
            <ul>
                <li><a class="index" href="/index">Index</a></li>
                <li><a class="mischelp" href="/misc/help">Help</a></li>
                <li><a class="users" href="/users">Users</a></li>
            </ul>

            <style>
                <#assign sel="." + current_tab!/>
                ${sel} {
                    text-decoration: underline;
                }
            </style>
        </nav>
    </header>
</#macro>

<#macro sidebar>
    <aside>
        <#assign rposts = posts?reverse>
        <#list rposts as post>
        <section>
            <div class="header">
                Post ${post.id}
            </div>
            <div class="body">
                <@postbody text=post.text full=false/>
            </div>
            <div class="footer">
                <a href="/post?post_id=${post.id}">View all</a>
            </div>
        </section>
        </#list>
    </aside>
</#macro>

<#macro footer>
    <footer>
        <a href="#">Codeforces</a> &copy; 2010-2019 by Mike Mirzayanov
    </footer>
</#macro>

<#macro page>
    <!DOCTYPE html>
    <html lang="en">
    <head>
        <meta charset="UTF-8">
        <title>Codeforces</title>
        <link rel="stylesheet" type="text/css" href="/css/normalize.css">
        <link rel="stylesheet" type="text/css" href="/css/style.css">
        <link rel="icon" href="/favicon.ico" type="image/x-icon"/>
        <link rel="shortcut icon" href="/favicon.ico" type="image/x-icon"/>
    </head>
    <body>
    <@header/>
    <div class="middle">
        <@sidebar/>
        <main>
            <#nested/>
        </main>
    </div>
    <@footer/>
    </body>
    </html>
</#macro>

<#macro userlink user nameOnly>
    <a class="userlink${user.id}" href="/user?handle=${user.handle}">${user.handle}</a>
    <#if nameOnly==false>
        <style>
            <#assign select="." + "userlink${user.id}"!/>
            ${select} {
                color: ${user.color}
            }
        </style>
    </#if>

</#macro>

<#macro postbody text full>
    <#if full || text?length <= 250>
        <p>${text}</p>
    <#else>
        <p>${text?substring(0, 250)}...</p>
    </#if>
</#macro>

<#macro postview post full>
    <article>
        <div class="title"> <a href="post?post_id=${post.id}"> ${post.title} </a> </div>
        <div class="information"> By <@userlink user=findBy(users, "id", post.user_id) nameOnly=false /> </div>
        <div class="body">
            <@postbody text=post.text full=full/>
        </div>
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
</#macro>

<#function findBy items key id>
    <#list items as item>
        <#if item[key]==id>
            <#return item/>
        </#if>
    </#list>
</#function>

<#function hasNext items key id>
    <#list items as item>
        <#if item[key]==id>
            <#return item_index < items?size-1>
        </#if>
    </#list>
</#function>

<#function next items key id>
    <#list items as item>
        <#if item[key]==id && item_index < items?size-1>
            <#return items[item_index+ 1].handle/>
        </#if>
    </#list>
</#function>

<#function hasPrev items key id>
    <#list items as item>
        <#if item[key]==id>
            <#return item_index &gt; 0>
        </#if>
    </#list>
</#function>

<#function prev items key id>
    <#list items as item>
        <#if item[key]==id && item_index &gt; 0>
            <#return items[item_index-1].handle/>
        </#if>
    </#list>
</#function>