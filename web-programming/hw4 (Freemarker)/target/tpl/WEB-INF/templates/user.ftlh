<#-- @ftlvariable name="id" type="java.lang.Long" -->
<#-- @ftlvariable name="handle" type="java.lang.String" -->
<#import "common.ftl" as c/>
<link href="../../css/user-style.css" rel="stylesheet"/>

<@c.page>
    <#if handle ??>
        <#assign viewedUser = c.findBy(users, "handle", handle)!>
    <#elseif user_id ??>
        <#assign  viewedUser = c.findBy(users, "id", user_id)!/>
    </#if>

    <#if viewedUser.id ??>
        <div class="user-page">
            <div class="user-profile">
                <div class="container-left">
                    <@c.userlink user=viewedUser nameOnly=false/>

                    <ul>
                        <li>id: ${viewedUser.id} </li>
                        <li>handle: ${viewedUser.handle} </li>
                        <li>name: ${viewedUser.name}</li>
                        <li>posts count: <a href="posts?user_id=${viewedUser.id}">${viewedUser.postCnt}</a></li>
                    </ul>
                </div>

                <div class="container-right">
                    <div class="user-photo">
                        <img src="../../img/user.jpg"
                    </div>
                </div>
            </div>
        </div>

        <div class="arrows">
            <#if c.hasPrev(users, "handle", viewedUser.handle)==true>
                <a href="user?handle=${c.prev(users, "handle", viewedUser.handle)}"> &#10229; </a>
            <#else>
                <span>&#10229;</span>
            </#if>

            <#if c.hasNext(users, "handle", viewedUser.handle)==true>
                <a href="user?handle=${c.next(users, "handle", viewedUser.handle)}"> &#10230; </a>
            <#else>
                <span>&#10230;</span>
            </#if>
        </div>
    <#else>
        <h1>No such user</h1>
    </#if>
</@c.page>
