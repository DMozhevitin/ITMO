<#import "common.ftl" as c/>

<@c.page>
    <div class="datatable">
        <div class="caption">Users</div>
        <table>
            <thead>
            <tr>
                <th class="th-id">Id</th>
                <th class="th-user">User</th>
            </tr>
            </thead>
            <tbody>
            <#list users as u>
                <tr>
                    <td>${u.id}</td>
                    <td><@c.userlink user=c.findBy(users, "id", u.id) nameOnly=false /></td>
                </tr>
            </#list>
            </tbody>
        </table>
    </div>
</@c.page>
