<template>
    <div class="middle">
        <aside>
            <SidebarPost v-for="post in viewPosts" :post="post" :key="post.id"/>
        </aside>
        <main>
            <Index :posts="posts" v-if="page === 'Index'"/>
            <Enter v-if="page === 'Enter'"/>
            <Users v-if="page === 'Users'"/>
            <Register v-if="page === 'Register'"/>
            <AddPost v-if="page === 'AddPost'"/>
            <FullPost :user="user" :post="viewedPost" v-if="page.startsWith('Post/')" />
        </main>
    </div>
</template>
<script>
    import Index from './middle/Index';
    import Enter from './middle/Enter';
    import Register from './middle/Register';
    import SidebarPost from './SidebarPost';
    import Users from './middle/Users';
    import AddPost from './middle/AddPost'
    import FullPost from "./middle/FullPost";

    export default {
        name: "Middle",
        props: ["posts", "user"],
        data: function () {
            return {
                page: "Index",
                id: null,
            }
        },
        computed: {
            viewPosts: function () {
                return Object.values(this.posts).sort((a, b) => b.id - a.id).slice(0, 2);
            },
            viewedPost: function() {
                return this.posts.filter(p => p.id === this.id)[0];
            }
        },
        components: {
            FullPost,
            Index,
            Enter,
            Register,
            SidebarPost,
            Users,
            AddPost
        }, beforeCreate() {
            this.$root.$on("onChangePage", (page, id) => {
                this.page = page;
                this.id = id;
            });
        }
    }
</script>

<style scoped>

</style>
