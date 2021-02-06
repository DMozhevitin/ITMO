<#import "common.ftl" as c/>

<@c.page>
<ul>
    <#assign rposts = posts?reverse>
    <#list rposts as post>
        <@c.postview post=post full=false/>
    </#list>
</ul>
</@c.page>
