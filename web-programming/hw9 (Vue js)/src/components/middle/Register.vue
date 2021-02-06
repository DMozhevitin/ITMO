<template>
    <div class="register form-box">
        <div class="header">Register</div>
        <div class="body">
            <form @submit.prevent="onRegister">
                <div class="field">
                    <div class="name">
                        <label for="login">Login</label>
                    </div>
                    <div class="value">
                        <input id="login" name="login" v-model="login"/>
                    </div>
                </div>

                <div class="field">
                    <div class="name">
                        <label for="username">Name</label>
                    </div>
                    <div class="value">
                        <input id="username" name="username" v-model="username"/>
                    </div>
                </div>

                <div class="error">{{error}}</div>

                <div class="button-field">
                    <input type="submit" value="Register">
                </div>
            </form>
        </div>
    </div>
</template>

<script>
    export default {
        data: function() {
            return {
                login: "",
                username: "",
                error: ""
            }
        },
        name: "Register",
        beforeCreate() {
            this.$root.$on("onRegisterValidationError", (error) => {
                this.error = error;
            });
        },
        beforeMount() {
            this.login = "";
            this.username = "";
            this.error = "";
        }, methods: {
            onRegister: function () {
                this.$root.$emit("onRegister", this.login, this.username);
            }
        }
    }
</script>

<style scoped>
</style>
