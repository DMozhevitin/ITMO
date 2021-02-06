<#-- @ftlvariable name="users" type="ru.itmo.wp.domain.User[]" -->
<#-- @ftlvariable name="toggleDisabledForm" type="ru.itmo.wp.form.ToggleDisabledForm" -->
<#import "commons.ftlh" as c>

<@c.page>
    <div class="users datatable">
        <div class="caption">User</div>
        <table>
            <thead>
            <tr>
                <th>Id</th>
                <th>Login</th>
                <th>Created</th>
                <#if user??>
                    <th>Disable</th>
                </#if>
            </tr>
            </thead>
            <tbody>
            <#if users?size==0>
                <tr class="noData">
                    <td colspan="3">
                        No data
                    </td>
                </tr>
            <#else>
                <#list users as u>
                    <tr>
                        <td class="id">${u.id}</td>
                        <td class="login"><@c.userlink user=u/></td>
                        <td class="creationTime">${u.creationTime}</td>
                        <#if user??>
                            <td class="disabled">
                                <#if u != user!>
                                    <form method="post" action="">
                                        <input type="hidden" name="action" value="toggleDisabled"/>
                                        <input type="hidden" name="id" value="${u.id}"/>
                                        <#if u.disabled>
                                            <#assign val="Enable"/>
                                        <#else>
                                            <#assign val="Disable"/>
                                        </#if>

                                        <input type="hidden" name="currentValue"
                                               value="${val}"/>
                                        <input type="submit" value="${val}">
                                    </form>
                                </#if>
                            </td>
                        </#if>
                    </tr>
                </#list>
            </#if>
            </tbody>
        </table>
    </div>
</@c.page>
