<#import "common.ftl" as c>

<@c.page>
    <#assign rposts = posts?reverse>
    <#if user_id ??>
        <#list rposts as post>
            <#if post.user_id == user_id>
                <@c.postview post=post full=false/>
            </#if>
        </#list>
    <#else>
        <#list rposts as post>
            <@c.postview post=post full=false/>
        </#list>
    </#if>
</@c.page>