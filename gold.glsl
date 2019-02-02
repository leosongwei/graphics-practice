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

vec3 cube_map_convolution(vec3 normal, samplerCube environmentMap){
  float PI = 3.1415;

  vec3 irradiance = vec3(0.0);

  vec3 up    = vec3(0.0, 1.0, 0.0);
  vec3 right = cross(up, normal);
  up         = cross(normal, right);

  float sampleDelta = 1.0;
  float nrSamples = 0.0;
  for(float phi = 0.0; phi < 2.0 * PI; phi += sampleDelta){
    for(float theta = 0.0; theta < 0.5 * PI; theta += sampleDelta){
      // spherical to cartesian (in tangent space)
      vec3 tangentSample = vec3(sin(theta) * cos(phi),  sin(theta) * sin(phi), cos(theta));
      // tangent space to world
      vec3 sampleVec = tangentSample.x * right + tangentSample.y * up + tangentSample.z * normal;
      irradiance += texture(environmentMap, sampleVec).rgb * cos(theta) * sin(theta);
      nrSamples++;
    }
  }
  irradiance = PI * irradiance * (1.0 / float(nrSamples));
  return(irradiance);
}

void main(){
  vec3 light_color = vec3(1.0, 1.0, 1.0);
  vec3 light_pos = vec3(0.0, 10.0, 20.0);
  vec3 light_dir = normalize(light_pos - frag_pos);
  vec3 view_pos = -vec3(trans_view[3]);

  // specular
  vec3 view_direction = normalize(view_pos - frag_pos);
  vec3 reflect_direction = reflect(-view_direction, normal);
  float spec = pow(max(dot(view_direction, reflect_direction), 0.0), mat_shininess);
  vec3 light_color_spec = 1.5 * vec3(texture(skybox, reflect_direction));
  vec3 specular = spec * light_color_spec * mat_specular;

  // diffuse
  vec3 diffuse = 2.0 * mat_diffuse * max(dot(normal, cube_map_convolution(normal, skybox)), 0.0);
  //vec3 diffuse = light_color * mat_diffuse * max(dot(normal, light_dir), 0.0);

  // ambient
  vec3 ambient = 0.1 * light_color * mat_ambient;

  color = vec3(specular + diffuse + ambient);
}
