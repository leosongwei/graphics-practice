// fragment shader

#version 330 core

in vec2 tex_coord;
in vec3 normal;
in vec3 frag_pos;

out vec3 color;
uniform sampler2D tex;
uniform samplerCube skybox;
uniform mat4 trans_view;

// material of gold
float mat_shininess = 0.3;
vec3 mat_ambient = vec3(0.24725, 0.1995, 0.0745);
vec3 mat_diffuse = vec3(0.75164, 0.60648, 0.22648);
vec3 mat_specular = vec3(0.628281, 0.555802, 0.366065);

void main(){
  vec3 light_color = vec3(1.0, 1.0, 1.0);
  vec3 light_pos = vec3(20.0, 20.0, 20.0);
  vec3 light_dir = normalize(light_pos - frag_pos);
  vec3 view_pos = -vec3(trans_view[3]);

  // specular
  vec3 view_direction = normalize(view_pos - frag_pos);
  vec3 reflect_direction = reflect(-light_dir, normal);
  float spec = pow(max(dot(view_direction, reflect_direction), 0.0), mat_shininess);
  vec3 light_color_spec = 1.5 * vec3(texture(skybox, reflect_direction));
  vec3 specular = spec * light_color_spec * mat_specular;

  // diffuse
  vec3 diffuse = light_color * mat_diffuse * max(dot(normal, light_dir), 0.0);

  // ambient
  vec3 ambient = 0.2 * light_color * mat_ambient;

  color = vec3(specular + diffuse + ambient);
}
