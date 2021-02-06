<template>
    <aside>
        <SidebarPost v-for="post in viewPosts" :post="post" :users="users" :key="post.id"/>
    </aside>
</template>

<script>
    import SidebarPost from "./SidebarPost";

    export default {
        name: "Sidebar",
        props: ['posts', 'users'],
        components: {
            SidebarPost
        },
        computed: {
            viewPosts: function () {
                return Object.values(this.posts).sort((a, b) => b.id - a.id).slice(0, 2);
            }
        }
    }
</script>

<style scoped>

</style>
